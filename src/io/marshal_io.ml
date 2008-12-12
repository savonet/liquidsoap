(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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

class virtual base ~pipe ~flags ~f ~g () = 
object(self)

  val mutable stream = None

  method virtual log : Dtools.Log.t

  method private close_pipe =
    match stream with
      | Some b -> g b;
                  stream <- None
      | None -> ()

  method private get_stream =
    match stream with
      | None ->
          if not (Sys.file_exists pipe) then
            Unix.mkfifo pipe 0o640
          else
           begin
            let stats = Unix.stat pipe in
            if stats.Unix.st_kind <> Unix.S_FIFO then
             begin
              self#log#f 1 "Pipe exists and is not a fifo.";
              raise (Lang.Invalid_value (Lang.string pipe,"pipe must be a fifo"))
             end
           end;
          let p = f flags 0o640 pipe in
          stream <- Some p;
          p
      | Some v -> v

end

class output ~pipe ~reopen val_source =
  let source = Lang.to_source val_source in
  let f = open_out_gen in
  let g = close_out in
  let flags = [Open_wronly;Open_binary] in
object (self)
  inherit Source.active_operator source
  inherit base ~pipe ~f ~g ~flags ()

  initializer
    (* We need the source to be infallible. *)
    if source#stype <> Source.Infallible then
      raise (Lang.Invalid_value (val_source, "That source is fallible"))

  method stype = Source.Infallible
  method remaining = source#remaining
  method get_frame buf = source#get buf
  method abort_track = source#abort_track

  method output_stop = self#close_pipe

  method output_reset = ()

  method output_get_ready = ignore(self#get_stream)

  method output =
    source#get memo;
    let pipe = self#get_stream in
    try
      Marshal.to_channel pipe memo []
    with
      | e ->
         self#log#f 1 "Could not write to pipe."; 
         if reopen then 
          begin
           self#log#f 1 "Will retry to open the pipe.";
           self#close_pipe
          end
         else
           raise e
end

class input ~pipe ~reopen () =
  let f = open_in_gen in
  let g = close_in in
  let flags = [Open_rdonly;Open_binary] in
object (self)
  inherit Source.source
  inherit base ~pipe ~f ~g ~flags ()

  initializer
    (* We are using blocking functions to read. *)
    (Dtools.Conf.as_bool (Configure.conf#path ["root";"sync"]))#set false

  method stype = Source.Infallible
  method remaining = -1
  method abort_track = ()

  method is_ready = true

  method get_frame frame =
    let pipe = self#get_stream in
    try
      Frame.fill_from_marshal pipe frame
    with
  (*  | Sys_blocked_io 
      | End_of_file -> 
          AFrame.blankify frame (Frame.position frame) (AFrame.size frame);
          Frame.add_break frame (Frame.size frame) *) (* Used for asynchronous communications.. *)
      | e ->
         self#log#f 1 "Could not read from pipe.";
         if reopen then
          begin
           self#log#f 1 "Will retry to open the pipe.";
           AFrame.blankify frame (Frame.position frame) (AFrame.size frame);
           Frame.add_break frame (Frame.size frame);
           self#close_pipe
          end
         else
           raise e
end

let () =
  Lang.add_operator "output.marshal"
    [
      "reopen", Lang.bool_t, Some (Lang.bool false),
      Some "Try to reopen the pipe after a failure.";
      "", Lang.string_t, None, 
      Some "Pipe to send the stream to.";
      "", Lang.source_t, None, None
    ]
    ~category:Lang.Output
    ~flags:[Lang.Experimental]
    ~descr:"Output the source's stream to a pipe using marshaling."
    (fun p ->
       let pipe = Lang.to_string (Lang.assoc "" 1 p) in
       let reopen = Lang.to_bool (List.assoc "reopen" p) in
       let source = Lang.assoc "" 2 p in
         ((new output ~pipe ~reopen source):>Source.source)
    );
  Lang.add_operator "input.marshal"
    [
      "reopen", Lang.bool_t, Some (Lang.bool false),
      Some "Try to reopen the pipe after a failure.";
      "", Lang.string_t, None,
      Some "Pipe to get the stream from.";
    ]
    ~category:Lang.Input
    ~flags:[Lang.Experimental]
    ~descr:"Get a stream from a pipe using marshaling."
    (fun p ->
       let pipe = Lang.to_string (List.assoc "" p) in
       let reopen = Lang.to_bool (List.assoc "reopen" p) in
       ((new input ~pipe ~reopen () ):>Source.source)
    );
