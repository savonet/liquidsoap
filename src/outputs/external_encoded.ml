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

(** Output a stream to an icecast server using an external encoder *)

type external_encoder = in_channel*out_channel

let priority = Tutils.Non_blocking

exception External_failure

let proto = 
  [  "process",
      Lang.fun_t
       [false,"",
        Lang.list_t (Lang.product_t Lang.string_t Lang.string_t)] 
      Lang.string_t, None, Some "External encoding process. \
                                 Takes initial metadata and return \
                                 the command to start.";
     "header",
      Lang.bool_t, Some (Lang.bool true), Some "Write wav header at \
                 beginning of encoded input.";
     "restart_on_crash",
     Lang.bool_t, Some (Lang.bool false),
     Some "Restart external process when it crashed. If false, \
           liquidsoap exits.";
     "restart_encoder", Lang.bool_t, Some (Lang.bool false), 
      Some "Restart encoder on new track." ]

let initial_meta =
  ["title","Liquidsoap stream";"artist","The Savonet Team"]
let m = 
  let m = Hashtbl.create 10 in
  List.iter
    (fun (x,y) ->
      Hashtbl.add m x y)
  initial_meta;
  m
let initial_meta = m

class virtual base ~restart_encoder ~restart_on_crash ~header process =
  let (out_p,in_p) = Unix.pipe () in
  let cond_m = Mutex.create () in
  let cond = Condition.create () in
object (self)

  method virtual log : Dtools.Log.t

  val virtual mutable encoder : external_encoder option

  val read_m = Mutex.create ()
  val read = Buffer.create 10
  val create_m = Mutex.create ()

  method encode frame start len =
      let b = AFrame.get_float_pcm frame in
      let start = Fmt.samples_of_ticks start in
      let len = Fmt.samples_of_ticks len in
      let slen = 2 * len * Array.length b in
      let sbuf = String.create slen in
      ignore(Float_pcm.to_s16le b start len sbuf 0);
      let (_,out_e) = 
        match encoder with
          | Some e -> e
          | None ->
               Mutex.lock create_m;
               match encoder with
                 | Some e -> 
		    (* Encoder was created elsewhere.. *)
		    Mutex.unlock create_m;
		    e
                 | None -> 
                     self#log#f 2 "No encoder available, starting process.";
                     self#external_start initial_meta;
                     Mutex.unlock create_m;
                     Utils.get_some encoder
      in
      begin 
        try
          output_string out_e sbuf;
        with
          | _ -> self#external_reset_on_crash initial_meta
      end;
      Mutex.lock read_m; 
      let ret = Buffer.contents read in
      Buffer.reset read;
      Mutex.unlock read_m;
      ret

  method private external_reset_encoder ?(crash=false) meta = 
    Mutex.lock read_m; 
    let ret = Buffer.contents read in
    Buffer.reset read;
    Mutex.unlock read_m;
    if restart_encoder || crash then 
      begin
        Mutex.lock create_m;
        self#external_stop;
        self#external_start meta;
	Mutex.unlock create_m
      end;
    ret

  method external_reset_on_crash meta =
    if restart_on_crash then
     begin 
      let ret = self#external_reset_encoder ~crash:true meta in
      Mutex.lock read_m; 
      Buffer.add_string read ret;
      Mutex.unlock read_m
     end
    else
      raise External_failure

  method reset_encoder = self#external_reset_encoder ~crash:false

  (* Any call of this function should be protected 
   * by a mutex. After locking the mutex, it should be
   * checked that the encoder was not create by another
   * call. *)
  method private external_start meta =
    self#log#f 2 "Creating external encoder..";
    let process = 
      Lang.to_string (Lang.apply process ["",Lang.metadata meta]) 
    in
    (* output_start must be called with encode = None. *)
    assert(encoder = None);
    let (in_e,out_e as enc) = Unix.open_process process in
    encoder <- Some enc;
    let sock = Unix.descr_of_in_channel in_e in
    let buf = String.create 10000 in
    let events = [`Read sock; `Read out_p]
    in
    let rec pull l =
      let read () =
        let ret = 
          try
            input in_e buf 0 10000
          with
            | _ -> self#external_reset_on_crash initial_meta;
                   0
        in
        if ret > 0 then
          begin
            Mutex.lock read_m; 
            Buffer.add_string read (String.sub buf 0 ret);
            Mutex.unlock read_m
          end;
        ret
      in
      let stop l =
        self#log#f 4 "reading task exited: closing process.";
        if List.mem (`Read out_p) l then
          ignore(Unix.read out_p " " 0 1); 
        begin
         try
          ignore(Unix.close_process enc);
         with _ -> ()
        end;
        encoder <- None;
        (* Signal the end of the task *)
        Mutex.lock cond_m;
        Condition.signal cond;
        Mutex.unlock cond_m
      in
      if List.mem (`Read sock) l then
        begin
         let ret = read () in
         if ret > 0 then
           [{ Duppy.Task.
                priority = priority ;
                events   = events ;
                handler  = pull }]
         else
          begin
            self#log#f 4 "Reading task reached end of data";
            stop l; []
          end
        end
      else
        begin
         assert(List.mem (`Read out_p) l);
         stop l; []
        end
    in
    Duppy.Task.add Tutils.scheduler
      { Duppy.Task.
          priority = priority ;
          events   = events ;
          handler  = pull };
    if header then
      begin
        let header =
          Wav.header ~channels:(Fmt.channels ()) 
                     ~sample_rate:(Fmt.samples_per_second ()) 
                     ~sample_size:16
                     ~big_endian:false ~signed:true ()
        in
        (* Write WAV header *)
        output_string out_e header
      end

  (* Don't fail if the task has already exited, like in 
   * case of failure for instance.. *)
  method private external_stop =
    match encoder with
      | Some (_,out_e) ->
         begin
           Mutex.lock cond_m;
           try 
             begin
              try
               flush out_e
              with
                | _ -> ()
             end;
             (* Signal the end of reading to task *)
             ignore(Unix.write in_p " " 0 1);
             (* Wait for the end of the task *)
             Condition.wait cond cond_m;
             Mutex.unlock cond_m
           with e -> 
             Mutex.unlock cond_m;
             self#log#f 2 "couldn't stop the reading task.";
             raise e
         end
      | None -> ()
end

class to_file
  ~append ~perm ~dir_perm ~reload_delay 
  ~reload_predicate ~reload_on_metadata
  ~filename ~autostart ~process ~restart_encoder
  ~header ~restart_on_crash source =
object (self)
  inherit
    [external_encoder] Output.encoded
         ~name:filename ~kind:"output.file" ~autostart source
  inherit File_output.to_file
            ~reload_delay ~reload_predicate ~reload_on_metadata
            ~append ~perm ~dir_perm filename as to_file
  inherit base ~restart_encoder ~header ~restart_on_crash process as base

  method output_start =
    base#external_start initial_meta;
    to_file#file_start

  method output_stop =
    base#external_stop;
    to_file#file_stop

  method output_reset = self#output_stop; self#output_start 
end

let () =
  Lang.add_operator "output.file.external"
    ([ "start",
      Lang.bool_t, Some (Lang.bool true),
      Some "Start output threads on operator initialization." ] 
      @ proto  
      @ File_output.proto @ ["", Lang.source_t, None, None ])
    ~category:Lang.Output
    ~descr:"Output the source's stream as a file, using an external encoding process."
    (fun p ->
       let e f v = f (List.assoc v p) in
       let autostart = e Lang.to_bool "start" in
       let filename = Lang.to_string (Lang.assoc "" 1 p) in
       let source = Lang.assoc "" 2 p in
       let append = Lang.to_bool (List.assoc "append" p) in
       let perm = Lang.to_int (List.assoc "perm" p) in
       let process = List.assoc "process" p in
       let dir_perm = Lang.to_int (List.assoc "dir_perm" p) in
       let reload_predicate = List.assoc "reopen_when" p in
       let reload_delay = Lang.to_float (List.assoc "reopen_delay" p) in
       let reload_on_metadata =
         Lang.to_bool (List.assoc "reopen_on_metadata" p)
       in
       let restart_encoder = Lang.to_bool (List.assoc "restart_encoder" p) in
       let restart_on_crash = Lang.to_bool (List.assoc "restart_on_crash" p) in
       let header = Lang.to_bool (List.assoc "header" p) in
         ((new to_file ~filename
             ~append ~perm ~dir_perm ~reload_delay 
             ~reload_predicate ~reload_on_metadata
             ~autostart ~process ~header ~restart_encoder 
             ~restart_on_crash source):>Source.source))

class to_pipe
  ~process ~restart_encoder ~restart_on_crash
  ~header ~autostart source =
object (self)
  inherit
    [external_encoder] Output.encoded ~name:"" ~kind:"output.pipe" ~autostart source
  inherit base ~restart_encoder ~restart_on_crash ~header process as base

  method send _ = () 

  method output_start = base#external_start initial_meta

  method output_stop = base#external_stop

  method output_reset = self#output_stop; self#output_start
end

let () =
  Lang.add_operator "output.pipe.external"
    ([ "start",
      Lang.bool_t, Some (Lang.bool true),
      Some "Start output threads on operator initialization." ]
      @ proto
      @ ["", Lang.source_t, None, None ])
    ~category:Lang.Output
    ~descr:"Output the source's stream to an external process."
    (fun p ->
       let e f v = f (List.assoc v p) in
       let autostart = e Lang.to_bool "start" in
       let source = Lang.assoc "" 1 p in
       let process = List.assoc "process" p in
       let restart_encoder = Lang.to_bool (List.assoc "restart_encoder" p) in
       let restart_on_crash = Lang.to_bool (List.assoc "restart_on_crash" p) in
       let header = Lang.to_bool (List.assoc "header" p) in
         ((new to_pipe ~autostart ~process ~header ~restart_encoder 
                       ~restart_on_crash source):>Source.source))
