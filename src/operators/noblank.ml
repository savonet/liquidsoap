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

(** In this file [length]s are in samples, [threshold] are RMS (in [0.;1.]). *)


class virtual base ~length ~threshold = 
object(self)

  val mutable blank_len = 0

  method abort_track =
    blank_len <- 0

  method virtual on_noise : unit

  method check_blank s p0 = 
    let len = AFrame.position s - p0 in
    let rms = Float_pcm.rms (AFrame.get_float_pcm s) p0 len in
    let noise = ref false in
      Array.iter (fun r -> if r > threshold then noise := true) rms;
      if !noise then
        begin
          blank_len <- 0 ;
          self#on_noise
        end
      else
        if blank_len + len <= length then
          blank_len <- blank_len + len
        else
          blank_len <- length + 1

end

class on_blank ~length ~threshold handler source =
object (self)
  inherit operator [source]
  inherit base ~length ~threshold as base

  val mutable in_blank = false

  method stype = source#stype
  method is_ready = source#is_ready
  method abort_track = 
    source#abort_track ; 
    base#abort_track
  method remaining = source#remaining

  method on_noise = ()

  method get_frame ab =
    let p0 = AFrame.position ab in
      source#get ab ;
      if AFrame.is_partial ab || p0 > 0 then blank_len <- 0 else begin
        self#check_blank ab p0 ;
        if blank_len <= length then
          in_blank <- false
        else
          if not in_blank then begin
            in_blank <- true ;
            ignore (Lang.apply handler [])
          end
      end
end

let log = Dtools.Log.make ["noblank"]

class skip ~length ~threshold source =
object (self)
  inherit operator [source]
  inherit base ~length ~threshold as base

  method stype = source#stype
  method is_ready = source#is_ready
  method abort_track = 
    source#abort_track ; 
    base#abort_track
  method remaining = source#remaining

  method on_noise = ()

  method get_frame ab =
    let p0 = AFrame.position ab in
      source#get ab ;
      if AFrame.is_partial ab || p0 > 0 then blank_len <- 0 else begin
        self#check_blank ab p0 ;
        if blank_len > length then begin
          source#abort_track ;
          log#f 3 "Too much blank, abort track!"
        end
      end
end

class strip ~length ~threshold source =
object (self)
  inherit active_operator source
  inherit base ~length ~threshold as base

  val mutable stripping = false

  method stype = Fallible
  method is_ready = not stripping && source#is_ready
  method remaining = if stripping then 0 else source#remaining
  method abort_track =
    source#abort_track ;
    base#abort_track ;
    stripping <- false

  method on_noise = stripping <- false

  method get_frame ab =
    let p0 = AFrame.position ab in
    let b0 = AFrame.breaks ab in
      source#get ab ;
      if AFrame.is_partial ab || p0 > 0 then begin
        blank_len <- 0 ;
        stripping <- false
      end else begin
        self#check_blank ab p0 ;
        if blank_len > length then begin
          AFrame.set_breaks ab (p0::b0) ;
          stripping <- true
        end
      end

  method private output =
    if stripping && AFrame.is_partial memo then self#get_frame memo

  method output_get_ready = ()
  method output_reset = ()
end

class eat ~at_beginning ~length ~threshold source =
object (self)
  inherit operator [source] as super
  inherit base ~length ~threshold as base

  val mutable stripping = false
  val mutable beginning = true

  method stype = Fallible
  method is_ready = source#is_ready
  method remaining = source#remaining
  method abort_track =
    source#abort_track ;
    base#abort_track ;
    stripping <- false ;
    beginning <- true

  method on_noise = 
    stripping <- false ;
    beginning <- false

  method get_frame ab =
    let first = ref true in
    let breaks = ref (AFrame.breaks ab) in
      while !first || stripping do
        if not !first then AFrame.set_breaks ab !breaks;
        first := false;
        let p0 = AFrame.position ab in
          source#get ab ;
          if AFrame.is_partial ab then
            (
              blank_len <- 0 ;
              stripping <- false ;
              beginning <- true
            )
          else
            (
              self#check_blank ab p0 ;
              if blank_len > length && (beginning || not at_beginning) then
                stripping <- true
            )
      done
end

let proto =
  [ "threshold", Lang.float_t, Some (Lang.float (-40.)),
    Some "Power in decibels under which the stream is considered silent." ;
    "length", Lang.float_t, Some (Lang.float 20.),
    Some "Maximum silence length allowed, in seconds." ;
    "", Lang.source_t, None, None ]

let extract p =
  let f v = List.assoc v p in
  let s = Lang.to_source (f "") in
  let length =
    let l = Lang.to_float  (f "length") in
      Fmt.samples_of_seconds l
  in
  let threshold =
    let v = f "threshold" in
    let t = Lang.to_float v in
      if t>0. then
        raise (Lang.Invalid_value (v,"threshold should be negative")) ;
      Sutils.lin_of_dB t
  in
    length,threshold,s

let () =
  Lang.add_operator "on_blank"
    ~category:Lang.TrackProcessing
    ~descr:"Calls a given handler when detecting a blank."
    (("",Lang.fun_t [] Lang.unit_t, None, None)::proto)
    (fun p ->
       let f = Lang.assoc "" 1 p in
       let p = List.remove_assoc "" p in
       let length,threshold,s = extract p in
         ((new on_blank ~length ~threshold f s):>source)) ;
  Lang.add_operator "skip_blank"
    ~category:Lang.TrackProcessing
    ~descr:"Skip track when detecting a blank."
    proto
    (fun p ->
       let length,threshold,s = extract p in
         ((new skip ~length ~threshold s):>source)) ;
  Lang.add_operator "strip_blank"
    ~category:Lang.TrackProcessing
    ~descr:"Make the source unavailable when it is streaming blank."
    proto
    (fun p ->
       let length,threshold,s = extract p in
         ((new strip ~length ~threshold s):>source));
  Lang.add_operator "eat_blank"
    ~category:Lang.TrackProcessing
    ~descr:"Eat blanks (i.e. drop the contents of the stream until it is not blank again)."
    (("at_beginning", Lang.bool_t, Some (Lang.bool false),
      Some "Only eat at the beginning of a track.")::proto)
    (fun p ->
       let at_beginning = Lang.to_bool (List.assoc "at_beginning" p) in
       let length,threshold,s = extract p in
         ((new eat ~at_beginning ~length ~threshold s):>source))
