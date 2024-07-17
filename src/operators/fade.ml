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

(** Fade-in at the beginning of every frame.
  * If the initial flag is set, only the first/current track is faded in. *)
class fade_in ?(initial=false) duration source =
object (self)

  inherit operator [source] as super

  method stype = source#stype
  method is_ready = source#is_ready
  method abort_track = source#abort_track

  val mutable count = 0
  val length = int_of_float (duration /. Mixer.Buffer.length)

  method get_frame ab =
    let offset1 = Mixer.Buffer.position ab in
      source#get ab ;
      count <- count+1 ;
      if Mixer.Buffer.is_partial ab && not initial then
        count <- 0 ;
      let offset2 = Mixer.Buffer.position ab in
        if count < length then
          Mixer.Buffer.change_volume ab offset1 (offset2-offset1) 
            ((float count)/.(float length))

end

(** Fade-out after every frame.
  * If the final flag is set, the fade-out happens as of instantiation
  * and the source becomes unavailable once it's finished. *)
class fade_out ?(final=false) duration source =
  let length = int_of_float (duration /. Mixer.Buffer.length) in
object (self)

  inherit operator [source] as super

  method stype = if final then Fallible else source#stype
  method abort_track = source#abort_track

  val mutable remaining = length
  method remaining =
    if final then remaining else source#remaining
  method is_ready = remaining > 0 && source#is_ready

  method get_frame ab =
    if remaining > 0 then
      let offset1 = Mixer.Buffer.position ab in
      let offset2 = source#get ab ; Mixer.Buffer.position ab in
        match
          if final then begin
            remaining <- remaining - 1 ;
            Some (remaining + 1)
          end else
            let n = source#remaining in
              if n>=0 && n<=length then Some n else None
        with
          | Some n ->
              Mixer.Buffer.change_volume ab offset1 (offset2-offset1) 
                ((float n)/.(float length))
          | None -> ()
    else
      Mixer.Buffer.add_break ab (Mixer.Buffer.position ab)

end

(** Lang interface *)

let proto =
  [ "duration", Lang.float_t, Some (Lang.float 3.), None ;
    "", Lang.source_t, None, None ]

let extract p =
  Lang.to_float (List.assoc "duration" p),
  Lang.to_source (List.assoc "" p)

let _ =
  Lang.add_operator "fade.in" proto
    ~descr:"Fade the beginning of every track."
    (fun p ->
       let d,s = extract p in
         ((new fade_in d s):>source)) ;
  Lang.add_operator "fade.initial" proto
    ~descr:"Fade the beginning of a stream."
    (fun p ->
       let d,s = extract p in
         ((new fade_in ~initial:true d s):>source)) ;
  Lang.add_operator "fade.out" proto
    ~descr:"Fade the end of tracks."
    (fun p ->
       let d,s = extract p in
         ((new fade_out d s):>source)) ;
  Lang.add_operator "fade.final" proto
    ~descr:"Fade a stream to silence."
    (fun p ->
       let d,s = extract p in
         ((new fade_out ~final:true d s):>source))

