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

open Source

class reader ?(debug=false) pipe =
object (self)
  inherit Source.active_source

  method stype = Infallible
  method is_ready = true
  method remaining = -1

  method abort_track = ()

  method output_reset = ()
  method is_active = true
  method output_get_ready = ()

  method output = self#get_frame memo

  method private get_frame buf =
    if debug then self#log#f 5 "Reader: get frame";
    if Frame.is_partial buf then
      Frame.fill_from_marshal pipe buf
    else
      Frame.add_break buf (Frame.size buf)
end

class fork ?(debug=false) ?f (source : source) =
object (self)
  inherit operator [source] as super

  val pipe_in =
    let fd_in, fd_out = Unix.pipe () in
      Unix.in_channel_of_descr fd_in, Unix.out_channel_of_descr fd_out

  val pipe_out =
    let fd_in, fd_out = Unix.pipe () in
      Unix.in_channel_of_descr fd_in, Unix.out_channel_of_descr fd_out

  method private wake_up activation =
    if debug then self#log#f 5 "Forking";
    match Unix.fork () with
      | 0 ->
          let source =
            match f with
              | Some f ->
                  let reader = new reader ~debug (fst pipe_in) in
                  let reader = (reader :> Source.source) in
                    Lang.to_source (Lang.apply f ["", Lang.source reader])
              | None ->
                  source
          in
          let frame = Frame.make () in
            source#get_ready [(self :> Source.source)];
            while true do
              if debug then self#log#f 5 "Son: getting frame";
              source#get frame;
              if debug then self#log#f 5 "Son: got frame";
              Marshal.to_channel (snd pipe_out) (frame : Frame.t) [];
              if debug then self#log#f 5 "Son: sent frame";
              source#after_output;
              Frame.advance frame;
            done
      | pid ->
          ignore (Dtools.Init.at_stop (fun () -> Unix.kill pid Sys.sigkill));
          if f <> None then
            (
              let frame = Frame.make () in
                AFrame.blankify frame 0 (AFrame.size frame);
                if debug then self#log#f 5 "Father: send first frames";
                Frame.add_break frame (Frame.size frame);
                Marshal.to_channel (snd pipe_in) (frame : Frame.t) [];
                Frame.set_breaks frame [];
                Marshal.to_channel (snd pipe_in) (frame : Frame.t) [];
                if debug then self#log#f 5 "Father: wrote first frames";
            );
          super#wake_up activation

  method stype = source#stype

  method remaining = source#remaining

  method is_ready = source#is_ready

  method abort_track = source#abort_track

  method private get_frame buf =
    source#get buf;
    if f <> None then
      (
        if debug then self#log#f 5 "Father: send frame";
        Marshal.to_channel (snd pipe_in) (buf : Frame.t) [];
        if debug then self#log#f 5 "Father: wrote frame"
      );
    let tmp : Frame.t = Marshal.from_channel (fst pipe_out) in
    Frame.blit tmp 0 buf 0 (Frame.size tmp);
    if debug then self#log#f 5 "Father: got frame back";
end

let () =
  Lang.add_operator "fork"
    [
      "debug", Lang.bool_t, Some (Lang.bool false), None;
      "", Lang.fun_t [false, "", Lang.source_t] Lang.source_t, None, Some "Function to be launched in an external process.";
      "", Lang.source_t, None, Some "Source given as argument of the function."
    ]
    ~descr:"Compute a source in another process (useful for multiple cores, etc). The function should not access any other source!"
    ~category:Lang.TrackProcessing (* TODO: better category *)
    ~flags:[Lang.Experimental; Lang.Hidden]
    (fun p _ ->
       let debug = Lang.to_bool (List.assoc "debug" p) in
       let f = Lang.assoc "" 1 p in
       let src = Lang.to_source (Lang.assoc "" 2 p) in
         new fork ~debug ~f src)

let () =
  Lang.add_operator "fork.source"
    [
      "debug", Lang.bool_t, Some (Lang.bool false), None;
      "", Lang.source_t, None, Some "Source to be forked."
    ]
    ~descr:"Compute a source in another process \
            (useful for multiple cores, etc). The sources used for \
            computing the source shouldn't be used outside!"
    ~category:Lang.TrackProcessing (* TODO: better category *)
    ~flags:[Lang.Experimental; Lang.Hidden]
    (fun p _ ->
       let debug = Lang.to_bool (List.assoc "debug" p) in
       let src = Lang.to_source (List.assoc "" p) in
         new fork ~debug src)
