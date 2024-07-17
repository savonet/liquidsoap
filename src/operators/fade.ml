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

open Source

(** Fade-in at the beginning of every frame.
  * If the initial flag is set, only the first/current track is faded in. *)
class fade_in ?(initial=false) duration source =
object (self)

  inherit operator [source] as super

  method stype = source#stype
  method is_ready = source#is_ready
  method abort_track = source#abort_track
  method remaining = source#remaining

  val mutable state = `Idle

  method get_frame ab =
    let p1 = Mixer.Buffer.position ab in
    let p2 = source#get ab ; Mixer.Buffer.position ab in
    let length,count =
      match state with
        | `Idle ->
            let duration =
              match Mixer.Buffer.get_metadata ab p1 with
                | None -> duration
                | Some m ->
                    match Utils.hashtbl_get m "liq_fade_in" with
                      | Some d -> (try float_of_string d with _ -> duration)
                      | None -> duration
            in
              (int_of_float (duration /. Mixer.Buffer.length)), 0
        | `Play (length,count) -> length, count+1
    in
      state <-
        if not initial && Mixer.Buffer.is_partial ab then
          `Idle
        else
          `Play (length,count) ;
      if count < length then
        Mixer.Buffer.change_volume ab p1 (p2-p1) 
          ((float count)/.(float length))

end

(** Fade-out after every frame.
  * If the final flag is set, the fade-out happens as of instantiation
  * and the source becomes unavailable once it's finished. *)
class fade_out ?(final=false) duration source =
object (self)

  inherit operator [source] as super

  method stype = if final then Fallible else source#stype
  method abort_track = source#abort_track

  (* Fade-out length (in frames) for the current track. *)
  val mutable cur_length = None

  (* Remaining frames, used only in final mode, untouched otherwise. *)
  val mutable remaining = int_of_float (duration /. Mixer.Buffer.length)

  method remaining =
    if final then remaining else source#remaining
  method is_ready = remaining > 0 && source#is_ready

  method get_frame ab =
    if remaining > 0 then
      let offset1 = Mixer.Buffer.position ab in
      let offset2 = source#get ab ; Mixer.Buffer.position ab in
      let length =
        match cur_length with
          | Some l -> l
          | None ->
              (* Set the length at the beginning of a track *)
              let duration =
                match Mixer.Buffer.get_metadata ab offset1 with
                  | None -> duration
                  | Some m ->
                      match Utils.hashtbl_get m "liq_fade_out" with
                        | None -> duration
                        | Some d -> (try float_of_string d with _ -> duration)
              in
              let l = int_of_float (duration /. Mixer.Buffer.length) in
                cur_length <- Some l ;
                l
      in
        (* Process the buffer *)
        begin match
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
        end ;
        (* Reset the length at the end of a track *)
        if Mixer.Buffer.is_partial ab then cur_length <- None
    else
      (* This happens in final mode at the end of the remaining time. *)
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
    ~descr:("Fade the beginning of tracks. Metadata 'liq_fade_in' can be used "^
            "to set the duration for a specific track (float in seconds).")
    (fun p ->
       let d,s = extract p in
         ((new fade_in d s):>source)) ;
  Lang.add_operator "fade.initial" proto
    ~descr:"Fade the beginning of a stream."
    (fun p ->
       let d,s = extract p in
         ((new fade_in ~initial:true d s):>source)) ;
  Lang.add_operator "fade.out" proto
    ~descr:("Fade the end of tracks. Metadata 'liq_fade_out' can be used to "^
            "set the duration for a specific track (float in seconds).")
    (fun p ->
       let d,s = extract p in
         ((new fade_out d s):>source)) ;
  Lang.add_operator "fade.final" proto
    ~descr:"Fade a stream to silence."
    (fun p ->
       let d,s = extract p in
         ((new fade_out ~final:true d s):>source))

