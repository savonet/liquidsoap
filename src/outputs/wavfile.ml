(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

(** Output in a WAV file. *)

class output name source start =
object
  inherit Output.output ~name ~kind:"output.wav" source start

  val mutable fd = None

  method output_start =
    let f = open_out name in
      output_string f (Wav.header Mixer.Buffer.format) ;
      fd <- Some f

  method output_stop =
    match fd with None -> assert false | Some f -> close_out f

  method output_send wav =
    let fd = match fd with None -> assert false | Some f -> f in
      output_string fd (Mixer.Buffer.to_string wav)

  method output_reset = ()
end

let _ =
  Lang.add_operator "output.wav"
    [ "start", Lang.bool_t, Some (Lang.bool true), None;
      "", Lang.string_t, None, None;
      "", Lang.source_t, None, None ]
    ~descr:"Output the source's stream to a WAV file."
    (fun p ->
       let autostart = Lang.to_bool (List.assoc "start" p) in
       let name = Lang.to_string (Lang.assoc "" 1 p) in
       let source = Lang.assoc "" 2 p in
         ((new output name source autostart):>Source.source))
