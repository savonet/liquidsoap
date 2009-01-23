(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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
class fade_in ?(meta="liq_video_fade_in") ?(initial=false) duration fader fadefun source =
object (self)

  inherit operator [source] as super

  method stype = source#stype
  method is_ready = source#is_ready
  method abort_track = source#abort_track
  method remaining = source#remaining

  val mutable state = `Idle

  method get_frame ab =
    let p1 = VFrame.position ab in
    let p2 = source#get ab ; VFrame.position ab in
    (** In video frames: [length] of the fade, [count] since beginning. *)
    let fade,length,count =
      match state with
        | `Idle ->
            let duration =
              match VFrame.get_metadata ab p1 with
                | None -> duration
                | Some m ->
                    match Utils.hashtbl_get m meta with
                      | Some d -> (try float_of_string d with _ -> duration)
                      | None -> duration
            in
            let length = Fmt.video_frames_of_seconds duration in
              fader length,
              length,
              0
        | `Play (fade,length,count) -> fade,length,count
    in
    let rgb = VFrame.get_rgb ab in
      if count < length then
        for i=0 to min (p2-p1-1) (length-count-1) do
          let m = fade (count+i) in
            for c = 0 to Array.length rgb - 1 do
              fadefun rgb.(c).(p1+i) m
            done
        done ;
      state <- (if not initial && VFrame.is_partial ab then
                  `Idle
                else
                  `Play (fade,length,count+p2-p1))

end

(** Fade-out after every frame.
  * If the final flag is set, the fade-out happens as of instantiation
  * and the source becomes unavailable once it's finished. *)
class fade_out ?(meta="liq_video_fade_out") ?(final=false) duration fader fadefun source =
object (self)

  inherit operator [source] as super

  method stype = if final then Fallible else source#stype
  method abort_track = source#abort_track

  (* Fade-out length (in video frames) for the current track.
   * The value is set at the beginning of every track, depending on metadata. *)
  val mutable cur_length = None

  (* Remaining frames, used only in final mode, untouched otherwise. *)
  val mutable remaining = Fmt.video_frames_of_seconds duration

  method remaining =
    if final then Fmt.ticks_of_video_frames remaining else source#remaining
  method is_ready = (remaining > 0 || not final) && source#is_ready

  method get_frame ab =
    if final && remaining <= 0 then
      (* This happens in final mode at the end of the remaining time. *)
      Frame.add_break ab (Frame.position ab)
    else
      let n = Fmt.video_frames_of_ticks source#remaining in
      let offset1 = VFrame.position ab in
      let offset2 = source#get ab ; VFrame.position ab in
      (** In video frames: [length] of the fade. *)
      let fade,length =
        match cur_length with
          | Some (f,l) -> f,l
          | None ->
              (* Set the length at the beginning of a track *)
              let duration =
                match VFrame.get_metadata ab offset1 with
                  | None -> duration
                  | Some m ->
                      match Utils.hashtbl_get m meta with
                        | None -> duration
                        | Some d -> (try float_of_string d with _ -> duration)
              in
              let l = Fmt.video_frames_of_seconds duration in
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
              let buffer = VFrame.get_rgb ab in
                for i=0 to offset2-offset1-1 do
                  let m = fade (n-i) in
                    for c=0 to Array.length buffer - 1 do
                      fadefun buffer.(c).(offset1+i) m
                    done
                done
          | None -> ()
        end ;
        if final then remaining <- remaining - offset2 + offset1 ;
        (* Reset the length at the end of a track *)
        if Frame.is_partial ab then cur_length <- None

end

(** Lang interface *)

(* TODO: share more with fade.ml *)
let proto =
  [
    "duration", Lang.float_t, Some (Lang.float 3.),
     Some "Duration of the fading. \
           This value can be set on a per-file basis using the metadata field \
           passed as override." ;
    "transition", Lang.string_t, Some (Lang.string "fade"),
    Some "Kind of transition (fade|slide_left|slide_right|slide_up|slide_down|grow|disc|random).";
    "type", Lang.string_t, Some (Lang.string "lin"),
    Some "Fader shape (lin|sin|log|exp): \
          linear, sinusoidal, logarithmic or exponential." ;
    "", Lang.source_t, None, None
  ]

let rec transition_of_string p transition =
  let ifm n a = int_of_float ((float_of_int n) *. a) in
    match transition with
      | "fade" -> RGB.scale_opacity
      | "slide_left" -> fun buf t -> RGB.translate buf (ifm (Fmt.video_width ()) (t-.1.)) 0
      | "slide_right" -> fun buf t -> RGB.translate buf (ifm (Fmt.video_width ()) (1.-.t)) 0
      | "slide_up" -> fun buf t -> RGB.translate buf 0 (ifm (Fmt.video_height ()) (1.-.t))
      | "slide_down" -> fun buf t -> RGB.translate buf 0 (ifm (Fmt.video_height ()) (t-.1.))
      | "grow" -> fun buf t -> RGB.affine buf t t 0 0
      | "disc" ->
          let w, h = Fmt.video_width (), Fmt.video_height () in
          let r_max = int_of_float (sqrt (float_of_int (w * w + h * h))) / 2 in
            fun buf t -> RGB.disk_opacity buf (w/2) (h/2) (ifm r_max t)
      | "random" ->
          let trans =
            [|"fade"; "slide_left"; "slide_right"; "slide_up"; "slide_down"; "grow"; "disc"|]
          in
            transition_of_string p trans.(Random.int (Array.length trans))
      | _ -> raise (Lang.Invalid_value (List.assoc "transition" p, "Invalid transition kind"))

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
  (let transition = Lang.to_string (List.assoc "transition" p) in
     transition_of_string p transition
  ),
  Lang.to_source (List.assoc "" p)

let override_doc =
  Some "Metadata field which, if present and containing a float, \
        overrides the 'duration' parameter for current track." 

let () =
  Lang.add_operator
    "video.fade.in"
    (("override", Lang.string_t, Some (Lang.string "liq_video_fade_in"),
      override_doc) :: proto)
    ~category:Lang.VideoProcessing
    ~descr:"Fade the beginning of tracks. Metadata 'liq_video_fade_in' \
            can be used to set the duration for a specific track \
            (float in seconds)."
    (fun p ->
       let d,f,t,s = extract p in
       let meta = Lang.to_string (List.assoc "override" p) in
         ((new fade_in ~meta d f t s):>source)) ;
  Lang.add_operator "video.fade.initial" proto
    ~category:Lang.VideoProcessing
    ~descr:"Fade the beginning of a stream."
    (fun p ->
       let d,f,t,s = extract p in
         ((new fade_in ~initial:true d f t s):>source)) ;
  Lang.add_operator
    "video.fade.out"
    (("override", Lang.string_t, Some (Lang.string "liq_video_fade_out"),
      override_doc) :: proto)
    ~category:Lang.VideoProcessing
    ~descr:"Fade the end of tracks. Metadata 'liq_video_fade_out' \
            can be used to set the duration for a specific track \
            (float in seconds)."
    (fun p ->
       let d,f,t,s = extract p in
       let meta = Lang.to_string (List.assoc "override" p) in
         ((new fade_out ~meta d f t s):>source)) ;
  Lang.add_operator "video.fade.final" proto
    ~category:Lang.VideoProcessing
    ~descr:"Fade a stream to black."
    (fun p ->
       let d,f,t,s = extract p in
         ((new fade_out ~final:true d f t s):>source))
