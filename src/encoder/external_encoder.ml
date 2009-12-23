(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(** External encoder *)

open Encoder.External

type external_encoder = in_channel*out_channel

type ext_handle = 
  { 
    cond_m          : Mutex.t ;
    cond            : Condition.t ;
    mutable is_task : bool ;
    mutable encoder : external_encoder option ;
    converter       : Audio_converter.Samplerate.t ;
    read_m          : Mutex.t ;
    read            : Buffer.t ;
    create_m        : Mutex.t ;
    log             : Dtools.Log.t ;
    params          : Encoder.External.t
  }

let create_handle id params = 
  { 
    cond_m    = Mutex.create () ;
    cond      = Condition.create () ;
    is_task   = false ;
    encoder   = None ;
    converter = Audio_converter.Samplerate.create params.channels ;
    read_m    = Mutex.create () ;
    read      = Buffer.create 10 ;
    create_m  = Mutex.create () ;
    log       = Dtools.Log.make [id] ;
    params    = params
  }

let priority = Tutils.Non_blocking

exception External_failure

let encoder id ext =
  (* The params *)
  let handle = create_handle id ext in
  let ratio =
    (float handle.params.samplerate) /. (float (Frame.audio_of_seconds 1.))
  in

  (* The encoding logic *)
  let stop_process h = 
    match h.encoder with
      | Some (_,out_e as enc) ->
         h.log#f 3 "stopping current process" ;
         begin
           Mutex.lock h.cond_m;
           try
             begin
              try
               flush out_e;
               Unix.close (Unix.descr_of_out_channel out_e)
              with
                | _ -> ()
             end;
             if h.is_task then
               Condition.wait h.cond h.cond_m;
             Mutex.unlock h.cond_m;
             begin
              try
                ignore(Unix.close_process enc)
              with
                | _ -> ()
             end;
             h.encoder <- None
           with
             | e ->
                 h.log#f 2 "couldn't stop the reading task.";
                 raise e
         end
      | None -> ()
  in

  let reset_process start_f h = 
    Mutex.lock h.create_m;
    try
      stop_process h;
      start_f h;
      Mutex.unlock h.create_m
    with
      | e ->
           Mutex.unlock h.create_m;
           raise e
  in

  (* This function does NOT use the 
   * create_m mutex. *)
  let rec start_process h = 
    assert(not h.is_task);
    h.log#f 2 "Creating external encoder..";
    let process = h.params.process in 
    (* output_start must be called with encode = None. *)
    assert(h.encoder = None);
    let (in_e,out_e as enc) = Unix.open_process process in
    h.encoder <- Some enc;
    if h.params.header then
      begin
        let header =
          Wav.header ~channels:h.params.channels
                     ~sample_rate:h.params.samplerate
                     ~sample_size:16
                     ~big_endian:false ~signed:true ()
        in
        (* Write WAV header *)
        output_string out_e header
      end;
    let sock = Unix.descr_of_in_channel in_e in
    let buf = String.create 10000 in
    let events = [`Read sock]
    in
    let rec pull _ =
      let read () =
        let ret = input in_e buf 0 10000 in
        if ret > 0 then
          begin
            Mutex.lock h.read_m; 
            Buffer.add_string h.read (String.sub buf 0 ret);
            Mutex.unlock h.read_m
          end;
        ret
      in
      let stop () =
        (* Signal the end of the task *)
        Mutex.lock h.cond_m;
        Condition.signal h.cond;
        h.is_task <- false;
        Mutex.unlock h.cond_m
      in
      try
        let ret = read () in
        if ret > 0 then
          [{ Duppy.Task.
               priority = priority ;
               events   = events ;
               handler  = pull }]
        else
         begin
           h.log#f 4 "Reading task reached end of data";
           stop (); []
         end
      with _ -> 
        stop (); 
        (if h.params.restart_on_crash then
          reset_process start_process h
         else
          raise External_failure) ;
        []
    in
    Duppy.Task.add Tutils.scheduler
      { Duppy.Task.
          priority = priority ;
          events   = events ;
          handler  = pull };
    h.is_task <- true;
    (** Creating restart task. *)
    match h.params.restart with
      | Delay d -> 
          let f _ =
            h.log#f 3 "Restarting encoder after delay (%is)" d;
            reset_process start_process h ;
            []
          in
          Duppy.Task.add Tutils.scheduler
            { Duppy.Task.
                priority = priority ;
                events   = [`Delay (float d)] ;
                handler  = f }
      | _ -> ()
  in

  let reset_process = reset_process start_process in

  let reset h _ = 
    if h.params.restart = Track then
      reset_process h;
    Mutex.lock h.read_m;
    let ret = Buffer.contents h.read in
    Buffer.reset h.read;
    Mutex.unlock h.read_m;
    ret
  in

  let encode h ratio frame start len =
    let channels = h.params.channels in
    let start = Frame.audio_of_master start in
    let b = AFrame.content_of_type ~channels frame start in
    let len = Frame.audio_of_master len in
    (* Resample if needed. *)
    let b,start,len =
      if ratio = 1. then
        b,start,len
      else
        let b =
          Audio_converter.Samplerate.resample
                 h.converter ratio b start len
        in
        b,0,Array.length b.(0)
    in
    let slen = 2 * len * Array.length b in
    let sbuf = String.create slen in
    ignore(Float_pcm.to_s16le b start len sbuf 0);
    (** Wait for any possible creation.. *)
    Mutex.lock h.create_m;
    begin
     match h.encoder with
       | Some (_,x) ->
           begin
            try
              output_string x sbuf;
              Mutex.unlock h.create_m
            with
              | _ ->
                Mutex.unlock h.create_m;
                if h.params.restart_on_crash then
                  reset_process h
                else
                  raise External_failure
           end
       | None ->
            Mutex.unlock h.create_m;
            raise External_failure
    end;
    Mutex.lock h.read_m;
    let ret = Buffer.contents h.read in
    Buffer.reset h.read;
    Mutex.unlock h.read_m;
    ret
  in

  let stop h () = 
    stop_process h ;
    (* Process stopped, no nead
     * to use the mutex. *)
    let ret = Buffer.contents h.read in
    Buffer.reset h.read;
    ret
  in
 
  (* Create an initial process *)
  start_process handle ;

  (* Return the encoding handle *)
  {
   Encoder. 
    reset  = reset handle ;
    encode = encode handle ratio ;
    stop   = stop handle ;
  }

let () =
  Encoder.plug#register "EXTERNAL"
    (function
       | Encoder.External m -> Some (fun s -> encoder s m)
       | _ -> None)
