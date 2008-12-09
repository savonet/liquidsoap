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
    assert(Frame.is_partial frame);
    let stream = Utils.get_some stream in
    let cur_pos = Frame.position frame in
    (* Yes, this might lose some data.
     * However, it is very simple this way,
     * and avoid either a local buffer or a 
     * send->receive paradigm with the other end.. *)
    let rec get () = 
      let (nframe : Frame.t) = Marshal.from_channel stream in
      if Frame.position nframe < cur_pos then
        get ()
      else
        nframe
    in
    let nframe = get () in
    let new_pos = Frame.position nframe in
    let len = new_pos - cur_pos in
    Frame.blit nframe cur_pos frame cur_pos len;
    let new_meta = Frame.get_all_metadata nframe in
    let cur_meta = Frame.get_all_metadata frame in
    let add_meta (p,m) = 
      match p with
        (* Last kept metadata should always be the more recent as possible.. *)
        | -1 -> 
             if not (List.mem_assoc (-1) 
                      (Frame.get_all_metadata frame)) 
             then
               Frame.set_metadata frame (-1) m
        | p when p >= cur_pos -> 
             Frame.set_metadata frame p m
        (** The perfectionist's addition:
          * Add a metadata in the worse case.. *)
        | p when
              (* No old metadata *) 
              cur_meta = [] && 
              (* No new metada, or kept metadata *)
              not (List.exists 
                    (fun (x,_) -> (x >= cur_pos) || (x = -1)) 
                        new_meta) &&
              (* No metadata was already added at cur_pos.. *)
              not (List.mem_assoc cur_pos 
                    (Frame.get_all_metadata frame)) 
                        ->
             (* Add this metadata at cur_pos. Since 
              * the list starts with the oldest one,
              * this should always add the latest one,
              * though I doubt another situation will
              * ever happen.. *) 
             Frame.set_metadata frame cur_pos m
        | _ -> ()
    in
    List.iter add_meta new_meta;
    Frame.add_break frame new_pos
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
