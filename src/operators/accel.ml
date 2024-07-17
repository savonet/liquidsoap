(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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

class accel ~before ~after ~ratio s =
object (self)
  inherit operator [s]

  method is_ready = s#is_ready
  method abort_track = s#abort_track
  method stype = s#stype

  val mutable elapsed = 0

  method remaining =
    let rem = s#remaining in
      if rem = -1 then -1 else
        if elapsed > before && rem > after then
          after + int_of_float (float (rem - after) *. ratio)
        else
          rem

  method private get_frame ab =
    let buf    = AFrame.get_float_pcm ab in
    let breaks = Frame.breaks ab in
    let rec get () =
      let start = Frame.position ab in
      let stop  = s#get ab ; Frame.position ab in
      let rem   = s#remaining in
        elapsed <- stop - start + elapsed ;
        if
          elapsed > before && (rem = -1 || rem > after)
          && (ratio *. float (stop-start) > 1.)
        then
          let data =
            Float_pcm.resample ratio buf start (stop-start)
          in
          let len = Array.length data.(0) in
            Array.iteri
              (fun i data -> Array.blit data 0 buf.(i) start len)
              data ;
            Frame.set_breaks ab (start+len::breaks) ;
            if Frame.is_partial ab then get ()
    in
      get () ;
      if Frame.is_partial ab then elapsed <- 0
end

let () =
  Lang.add_operator "accelerate"
    [ "", Lang.float_t, None, None ;

      "before", Lang.float_t, Some (Lang.float 10.),
      Some "Do not accelerate during the first <before> seconds." ;

      "after", Lang.float_t, Some (Lang.float 10.),
      Some "Do not accelerate during the last <after> seconds." ;

      "", Lang.source_t, None, None ]
    ~category:Lang.SoundProcessing
    ~descr:"Accelerates a stream, possibly only the middle of the tracks. Useful for testing transitions."
    (fun p ->
       let src    = Lang.to_source (Lang.assoc "" 2 p) in

       let before = Lang.to_float  (Lang.assoc "before" 1 p) in
       let after  = Lang.to_float  (Lang.assoc "after" 1 p) in

       let before = Fmt.ticks_of_seconds before in
       let after  = Fmt.ticks_of_seconds after in

       let ratio =
         let v = Lang.assoc "" 1 p in
         let s = Lang.to_float v in
           if s < 1. then
             raise (Lang.Invalid_value (v,"ratio should be more than 1.")) ;
           1. /. s
       in

         ((new accel ~before ~after ~ratio src):>source))
