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

class bpm (source:source) every =
object (self)
  inherit operator [source] as super

  method stype = source#stype
  method is_ready = source#is_ready
  method remaining = source#remaining
  method abort_track = source#abort_track

  val bpm = Soundtouch.BPM.make (Fmt.channels()) (Fmt.samples_per_second())

  val mutable n = 0

  method get_frame buf =
    let offset = AFrame.position buf in
      source#get buf;
      Soundtouch.BPM.put_samples_ni bpm (AFrame.get_float_pcm buf) offset ((AFrame.position buf) - offset);
      n <- n + 1;
      if n >= every then
        (
          n <- 0;
          Printf.printf "BPM: %.02f\n%!" (Soundtouch.BPM.get_bpm bpm)
        )

end

let () =
  Lang.add_operator "bpm"
    [
      "every", Lang.int_t, Some (Lang.int 500), None;
      "", Lang.source_t, None, None
    ]
    ~category:Lang.SoundProcessing
    ~descr:"Detect the BPM."
    ~flags:[Lang.Experimental]
    (fun p ->
       let f v = List.assoc v p in
       let every = Lang.to_int (f "every") in
       let s = Lang.to_source (f "") in
         ((new bpm s every):>source))
