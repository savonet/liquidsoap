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

class soundtouch (source:source) rate tempo pitch =
object (self)
  inherit operator [source] as super

  val st = Soundtouch.make (Fmt.channels()) (Fmt.samples_per_second())

  initializer
    self#log#f 3 "Using soundtouch %s." (Soundtouch.get_version_string st)

  method stype = source#stype
  method is_ready = source#is_ready
  method remaining = source#remaining
  method abort_track = source#abort_track

  (* Temporary buffer. *)
  val databuf = Fmt.create_frame ()

  method get_frame buf =
    let b = AFrame.get_float_pcm buf in
    let startpos = AFrame.position buf in
    let endpos = AFrame.size buf in
      Soundtouch.set_rate st (rate ());
      Soundtouch.set_tempo st (tempo ());
      Soundtouch.set_pitch st (pitch ());
      (* TODO: handle end of tracks *)
      while Soundtouch.get_available_samples st < endpos - startpos do
        AFrame.clear databuf;
        source#get databuf;
        let db = AFrame.get_float_pcm databuf in
          Soundtouch.put_samples_ni st db 0 (Array.length db.(0))
      done;
      ignore (Soundtouch.get_samples_ni st b startpos (endpos - startpos));
      AFrame.add_break buf (AFrame.size buf)
end

let () =
  Lang.add_operator "soundtouch"
    [
      "rate", Lang.float_getter_t 1, Some (Lang.float 1.0), None;
      "tempo", Lang.float_getter_t 2, Some (Lang.float 1.0), None;
      "pitch", Lang.float_getter_t 3, Some (Lang.float 1.0), None;
      "", Lang.source_t, None, None;
    ]
    ~category:Lang.SoundProcessing
    ~descr:"Change the rate, the tempo or the pitch of the sound."
    ~flags:[Lang.Experimental]
    (fun p ->
       let f v = List.assoc v p in
       let rate = Lang.to_float_getter (f "rate") in
       let tempo = Lang.to_float_getter (f "tempo") in
       let pitch = Lang.to_float_getter (f "pitch") in
       let s = Lang.to_source (f "") in
         ((new soundtouch s rate tempo pitch):>source))
