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

(** Fade-in at the beginning of every frame.
  * The [duration] is in seconds.
  * If the initial flag is set, only the first/current track is faded in. *)
class fade_in ?(meta="liq_fade_in") ?(initial=false) duration fader source =
object (self)

  inherit operator [source] as super

  method stype = source#stype
  method is_ready = source#is_ready
  method abort_track = source#abort_track
  method remaining = source#remaining

  val mutable state = `Idle

  method get_frame ab =
    let p1 = AFrame.position ab in
    let p2 = source#get ab ; AFrame.position ab in
    (** In samples: [length] of the fade, [count] since beginning. *)
    let fade,length,count =
      match state with
        | `Idle ->
            let duration =
              match AFrame.get_metadata ab p1 with
                | None -> duration
                | Some m ->
                    match Utils.hashtbl_get m meta with
                      | Some d -> (try float_of_string d with _ -> duration)
                      | None -> duration
            in
            let length = Fmt.samples_of_seconds duration in
              fader length,
              length,
              0
        | `Play (fade,length,count) -> fade,length,count
    in
    let pcm = AFrame.get_float_pcm ab in
      if count < length then
        for i=0 to min (p2-p1-1) (length-count-1) do
          let m = fade (count+i) in
            for c = 0 to Array.length pcm - 1 do
              pcm.(c).(p1+i) <- m *. pcm.(c).(p1+i)
            done
        done ;
      state <- (if not initial && AFrame.is_partial ab then
                  `Idle
                else
                  `Play (fade,length,count+p2-p1))

end

(** Fade-out after every frame.
  * If the final flag is set, the fade-out happens as of instantiation
  * and the source becomes unavailable once it's finished. *)
class fade_out ?(meta="liq_fade_out") ?(final=false) duration fader source =
object (self)

  inherit operator [source] as super

  method stype = if final then Fallible else source#stype
  method abort_track = source#abort_track

  (* Fade-out length (in samples) for the current track.
   * The value is set at the beginning of every track, depending on metadata. *)
  val mutable cur_length = None

  (* Remaining frames, used only in final mode, untouched otherwise. *)
  val mutable remaining = Fmt.samples_of_seconds duration

  method remaining =
    if final then Fmt.ticks_of_samples remaining else source#remaining
  method is_ready = (remaining > 0 || not final) && source#is_ready

  method get_frame ab =
    if final && remaining <= 0 then
      (* This happens in final mode at the end of the remaining time. *)
      Frame.add_break ab (Frame.position ab)
    else
      let n = Fmt.samples_of_ticks source#remaining in
      let offset1 = AFrame.position ab in
      let offset2 = source#get ab ; AFrame.position ab in
      (** In samples: [length] of the fade. *)
      let fade,length =
        match cur_length with
          | Some (f,l) -> f,l
          | None ->
              (* Set the length at the beginning of a track *)
              let duration =
                match AFrame.get_metadata ab offset1 with
                  | None -> duration
                  | Some m ->
                      match Utils.hashtbl_get m meta with
                        | None -> duration
                        | Some d -> (try float_of_string d with _ -> duration)
              in
              let l = Fmt.samples_of_seconds duration in
              let f = fader l in
                cur_length <- Some (f,l) ;
                f,l
      in
        (* Process the buffer *)
        begin match
          if final then Some remaining else
            if n>=0 && n<length then Some n else None
        with
          | Some n ->
              let buffer = AFrame.get_float_pcm ab in
                for i=0 to offset2-offset1-1 do
                  let m = fade (n-i) in
                    for c=0 to Array.length buffer - 1 do
                      buffer.(c).(offset1+i) <- m *. buffer.(c).(offset1+i)
                    done
                done
          | None -> ()
        end ;
        if final then remaining <- remaining - offset2 + offset1 ;
        (* Reset the length at the end of a track *)
        if Frame.is_partial ab then cur_length <- None

end

(** Lang interface *)

let proto =
  [ "duration", Lang.float_t, Some (Lang.float 3.), 
     Some "Duration of the fading. \
           This value can be set on a per-file basis using the metadata field \
           passed as override." ;
    "type", Lang.string_t, Some (Lang.string "lin"),
    Some "Fader shape (lin|sin|log|exp): \
          linear, sinusoidal, logarithmic or exponential." ;
    "", Lang.source_t, None, None ]

let extract p =
  Lang.to_float (List.assoc "duration" p),
  (let mode = List.assoc "type" p in
   let f =
     (* A few typical shapes..
      * In theory, any mapping from [0:1] to [0:1] is OK,
      * preferably monotonic and one-to-one. *)
     match Lang.to_string mode with
       | "lin" -> (fun x -> x)
       | "log" ->
           let curve = 10. in
           let m = log (1.+.curve) in
             (fun x -> log (1. +. x*.10.) /. m)
       | "exp" ->
           let curve = 2. in
           let m = exp curve -. 1. in
             (fun x -> (exp (curve *. x) -. 1.) /. m)
       | "sin" ->
           let pi = acos (-1.) in
             (fun x -> (1. +. sin ((x-.0.5)*.pi))/.2.)
       | _ ->
           let msg =
             "The 'type' parameter should be 'lin','sin','log' or 'exp'!"
           in
             raise (Lang.Invalid_value (mode,msg))
   in
     fun l ->
       let l = float l in
         fun i ->
           let i = float i /. l in
             f (max 0. (min 1. i))
  ),
  Lang.to_source (List.assoc "" p)

let override_doc = 
  Some "Metadata field which, if present and containing a float, \
        overrides the 'duration' parameter for current track." 

let () =
  Lang.add_operator 
    "fade.in" (("override", Lang.string_t, Some (Lang.string "liq_fade_in"),
               override_doc) :: proto)
    ~category:Lang.SoundProcessing
    ~descr:("Fade the beginning of tracks. Metadata 'liq_fade_in' can be used "^
            "to set the duration for a specific track (float in seconds).")
    (fun p ->
       let d,f,s = extract p in
       let meta = Lang.to_string (List.assoc "override" p) in
         ((new fade_in ~meta d f s):>source)) ;
  Lang.add_operator "fade.initial" proto
    ~category:Lang.SoundProcessing
    ~descr:"Fade the beginning of a stream."
    (fun p ->
       let d,f,s = extract p in
         ((new fade_in ~initial:true d f s):>source)) ;
  Lang.add_operator 
    "fade.out" (("override", Lang.string_t, Some (Lang.string "liq_fade_out"),
               override_doc) :: proto) 
    ~category:Lang.SoundProcessing
    ~descr:("Fade the end of tracks. Metadata 'liq_fade_out' can be used to "^
            "set the duration for a specific track (float in seconds).")
    (fun p ->
       let d,f,s = extract p in
       let meta = Lang.to_string (List.assoc "override" p) in
         ((new fade_out ~meta d f s):>source)) ;
  Lang.add_operator "fade.final" proto
    ~category:Lang.SoundProcessing
    ~descr:"Fade a stream to silence."
    (fun p ->
       let d,f,s = extract p in
         ((new fade_out ~final:true d f s):>source))
