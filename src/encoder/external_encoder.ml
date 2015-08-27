(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2015 Savonet team

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
        try
          begin
            try
              flush out_e;
              Unix.close (Unix.descr_of_out_channel out_e)
            with
            | _ -> ()
          end;
          Tutils.mutexify h.cond_m (fun () ->
            if h.is_task then
              Condition.wait h.cond h.cond_m) ();
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
    Tutils.mutexify h.create_m (fun () ->
      stop_process h;
      start_f h) ()
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
    if h.params.video then
      begin
        let header =
          Avi.header ~channels:h.params.channels ~samplerate:h.params.samplerate ()
        in
        output_string out_e header
      end
    else if h.params.header then
      begin
        let header =
          Wav_aiff.wav_header ~channels:h.params.channels
            ~sample_rate:h.params.samplerate
            ~sample_size:16 ()
        in
        (* Write WAV header *)
        output_string out_e header
      end;
    let sock = Unix.descr_of_in_channel in_e in
    let buf = Bytes.create 10000 in
    let events = [`Read sock]
    in
    let rec pull _ =
      let read () =
        let ret = input in_e buf 0 10000 in
        if ret > 0 then
          Tutils.mutexify h.read_m (fun () ->
            Buffer.add_string h.read (String.sub buf 0 ret)) ();
        ret
      in
      let stop () =
        (* Signal the end of the task *)
        Tutils.mutexify h.cond_m (fun () ->
          Condition.signal h.cond;
          h.is_task <- false) ();
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
      with e -> 
        h.log#f 3
          "Error while reading data from encoding process: %s"
          (Printexc.to_string e) ;
        stop (); 
        (if h.params.restart_on_crash then
            reset_process start_process h
         else
            raise e);
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

  let insert_metadata h _ = 
    if h.params.restart = Metadata then
      reset_process h;
  in

  let encode h ratio frame start len =
    let channels = h.params.channels in
    let sbuf =
      if h.params.video then
        Avi_encoder.encode_frame
          ~channels ~samplerate:h.params.samplerate ~converter:h.converter
          frame start len
      else
        (
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
          let sbuf = Bytes.create slen in
          Audio.S16LE.of_audio b start sbuf 0 len;
          sbuf
        )
    in
    (** Wait for any possible creation.. *)
    begin
      try
        Tutils.mutexify h.create_m (fun () ->
          match h.encoder with
          | Some (_,x) ->
             output_string x sbuf;
          | None ->
             raise Not_found) ()
      with
      | e ->
         h.log#f 3
           "Error while writing data to encoding process: %s"
           (Printexc.to_string e) ;
        if h.params.restart_on_crash then
          reset_process h
        else
          raise e
    end;
    Tutils.mutexify h.read_m (fun () ->
      let ret = Buffer.contents h.read in
      Buffer.reset h.read;
      ret) ()
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
    insert_metadata  = insert_metadata handle ;
    (* External encoders do not support 
     * headers for now. They will probably
     * never do.. *)
    header = None ;
    encode = encode handle ratio ;
    stop   = stop handle ;
  }

let () =
  Encoder.plug#register "EXTERNAL"
    (function
       | Encoder.External m -> Some (fun s _ -> encoder s m)
       | _ -> None)
