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

class output ~pipe val_source =
  let source = Lang.to_source val_source in
object (self)
  inherit Source.active_operator source

  initializer
    (* We need the source to be infallible. *)
    if source#stype <> Source.Infallible then
      raise (Lang.Invalid_value (val_source, "That source is fallible"))

  val mutable stream = None

  method stype = Source.Infallible
  method remaining = source#remaining
  method get_frame buf = source#get buf
  method abort_track = source#abort_track

  method output_get_ready =
    if stream = None then
      stream <- Some (open_out pipe)

  method output_reset = ()

  method output_stop = 
    match stream with
      | Some b -> close_out b;
                  stream <- None
      | None -> ()

  method output =
    source#get memo;
    let stream = Utils.get_some stream in
    Marshal.to_channel stream memo []
end

class input ~pipe () =
object (self)
  inherit Source.active_source

  initializer
    (* We are using blocking functions to read. *)
    (Dtools.Conf.as_bool (Configure.conf#path ["root";"sync"]))#set false

  val mutable stream = None

  method stype = Source.Infallible
  method remaining = -1
  method abort_track = ()
  method output = if AFrame.is_partial memo then self#get_frame memo

  method output_get_ready =
   if stream = None then
    stream <- Some (open_in_bin pipe)

  method output_reset = ()

  method output_stop =
    match stream with
      | Some b -> close_in b;
                  stream <- None
      | None -> ()

  method get_frame frame =
    assert (0 = AFrame.position frame) ;
    let stream = Utils.get_some stream in
    let (nframe : Frame.t) = Marshal.from_channel stream in
    Frame.blit nframe 0 frame 0 (Frame.position nframe);
    Frame.set_all_metadata frame (Frame.get_all_metadata nframe);
    Frame.add_break frame (Frame.position nframe)
end

let () =
  Lang.add_operator "output.marshal"
    [
      "", Lang.string_t, None, 
      Some "Pipe to send the stream to.";
      "", Lang.source_t, None, None
    ]
    ~category:Lang.Output
    ~flags:[Lang.Experimental]
    ~descr:"Output the source's stream to a pipe using marshaling."
    (fun p ->
       let pipe = Lang.to_string (Lang.assoc "" 1 p) in
       let source = Lang.assoc "" 2 p in
         ((new output ~pipe source):>Source.source)
    );
  Lang.add_operator "input.marshal"
    [
      "", Lang.string_t, None,
      Some "Pipe to get the stream from.";
    ]
    ~category:Lang.Input
    ~flags:[Lang.Experimental]
    ~descr:"Get a stream from a pipe using marshaling."
    (fun p ->
       let pipe = Lang.to_string (List.assoc "" p) in
       ((new input ~pipe () ):>Source.source)
    );
