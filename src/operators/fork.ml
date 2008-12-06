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

open Source

class reader pipe =
object (self)
  inherit Source.active_source

  method remaining = -1

  method output_reset = ()
  method output_get_ready = ()
  method abort_track = ()

  method output = self#get_frame memo

  method get_frame buf =
    Printf.printf "Reader: get frame\n%!";
    let frame = Marshal.from_channel pipe in
    let frame = (frame : Frame.t) in
      Printf.printf "Reader: got frame\n%!";
      (* Frame.copy_to frame buf *)
      Frame.get_chunk buf frame
end

class fork f (source : source) =
object (self)
  inherit operator [source] as super

  val pipe_in =
    let fd_in, fd_out = Unix.pipe () in
      Unix.in_channel_of_descr fd_in, Unix.out_channel_of_descr fd_out

  val pipe_out =
    let fd_in, fd_out = Unix.pipe () in
      Unix.in_channel_of_descr fd_in, Unix.out_channel_of_descr fd_out

  method wake_up activation =
    Printf.printf "Forking\n%!";
    if Unix.fork () = 0 then
      let reader = new reader (fst pipe_in) in
      let reader = (reader :> Source.source) in
      let source = Lang.to_source (Lang.apply f ["", Lang.source reader]) in
      let frame = Frame.make () in
        source#get_ready [(self :> Source.source)];
        while true do
          Printf.printf "Son: getting frame\n%!";
          source#get frame;
          Printf.printf "Son: got frame\n%!";
          Marshal.to_channel (snd pipe_out) (frame : Frame.t) [];
          flush (snd pipe_out);
          Printf.printf "Son: sent frame\n%!";
          source#after_output;
          Frame.advance frame;
        done
    else
      super#wake_up activation

  method stype = source#stype

  method remaining = source#remaining

  method is_ready = source#is_ready

  method abort_track = source#abort_track

  method get_frame buf =
    Printf.printf "Father: send frame\n%!";
    Marshal.to_channel (snd pipe_in) (buf : Frame.t) [];
    flush (snd pipe_in);
    Printf.printf "Father: wrote frame\n%!";
    Frame.copy_to (Marshal.from_channel (fst pipe_out) : Frame.t) buf;
    Printf.printf "Father: got frame back\n%!";
end

let () =
  Lang.add_operator "fork"
    [
      "", Lang.fun_t [false, "", Lang.source_t] Lang.source_t, None, None;
      "", Lang.source_t, None, None
    ]
    ~descr:"Compute a source in another process (useful for multiple cores, etc). The function should not access any other source!"
    ~category:Lang.TrackProcessing (* TODO: better category *)
    ~flags:[Lang.Experimental; Lang.Hidden]
    (fun p ->
       let f = Lang.assoc "" 1 p in
       let src = Lang.to_source (Lang.assoc "" 2 p) in
         ((new fork f src):>source)
    )
