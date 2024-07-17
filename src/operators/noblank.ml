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

open Types

class skip ~length ~threshold source =
object (self)
  inherit operator [source]

  val mutable blank_len = 0.

  method stype = source#stype
  method is_ready = source#is_ready
  method abort_track = source#abort_track ; blank_len <- 0.

  method check_blank s =
    if Mixer.Buffer.rms s > threshold then
      blank_len <- 0.
    else
      blank_len <- blank_len +. Mixer.Buffer.length

  method get_frame ab =
    let p0 = Mixer.Buffer.position ab in
      source#get ab ;
      if Mixer.Buffer.is_partial ab || p0 > 0 then blank_len <- 0. else begin
        self#check_blank ab ;
        if blank_len > length then begin
          source#abort_track ;
          Dtools.Log.log ~label:"noblank" 3 "Too much blank, abort track!"
        end
      end
end

class strip ~length ~threshold source =
object (self)
  inherit active_operator source

  val mutable blank_len = 0.
  val mutable stripping = false

  method stype = Fallible
  method is_ready = not stripping && source#is_ready
  method abort_track =
    source#abort_track ;
    blank_len <- 0. ;
    stripping <- false

  (** TODO Don't put back the stream for any noise, but use a nonblank_len *)
  method check_blank s =
    if Mixer.Buffer.rms s > threshold then
      ( blank_len <- 0. ; stripping <- false )
    else
      blank_len <- blank_len +. Mixer.Buffer.length

  method get_frame ab =
    let p0 = Mixer.Buffer.position ab in
    let b0 = Mixer.Buffer.breaks ab in
      source#get ab ;
      if Mixer.Buffer.is_partial ab || p0 > 0 then begin
        blank_len <- 0. ;
        stripping <- false
      end else begin
        self#check_blank ab ;
        if blank_len > length then begin
          Mixer.Buffer.set_breaks ab (p0::b0) ;
          stripping <- true
        end
      end

  method private output =
    if stripping && Mixer.Buffer.is_partial memo then self#get_frame memo
end

let proto =
  [ "threshold", Lang.float_t, Some (Lang.float 100.),
    Some
      "Intensity threshold under which the stream is considered to be blank." ;
    "length", Lang.float_t, Some (Lang.float 20.),
    Some "Maximum silence length allowed." ;
    "", Lang.source_t, None, None ]

let _ = 
  Lang.add_operator "skip_blank" ~descr:"Skip track when detecting a blank"
    proto
    (fun p ->
       let f v = List.assoc v p in
       let length    = Lang.to_float  (f "length") in
       let threshold = Lang.to_float  (f "threshold") in
       let s = Lang.to_source (f "") in
         ((new skip ~length ~threshold s):>source)) ;
  Lang.add_operator "strip_blank" ~descr:"Strip blanks"
    proto
    (fun p ->
       let f v = List.assoc v p in
       let length    = Lang.to_float  (f "length") in
       let threshold = Lang.to_float  (f "threshold") in
       let s = Lang.to_source (f "") in
         ((new strip ~length ~threshold s):>source))
