(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

open Source

(** Fade-in at the beginning of every track.
  * The [duration] is in seconds. *)
class fade_in ~kind ?(meta = "liq_video_fade_in") duration fader fadefun source
  =
  object
    inherit operator ~name:"video.fade.in" kind [source]

    method stype = source#stype

    method is_ready = source#is_ready

    method abort_track = source#abort_track

    method remaining = source#remaining

    method self_sync = source#self_sync

    val mutable state = `Idle

    method private get_frame ab =
      let off_ticks = Frame.position ab in
      let video_content = VFrame.get_content ab source in
      (* In video frames: [length] of the fade, [count] since beginning.
      * This must be done before accessing the (possibly empty video content)
      * because the state has to be updated anyway. Also, it is important
      * that the metadata is ready at the position in ticks rather than
      * video, otherwise we might miss some data. *)
      let fade, fadefun, length, count =
        match state with
          | `Idle ->
              let duration =
                match Frame.get_metadata ab off_ticks with
                  | None -> duration
                  | Some m -> (
                      match Utils.hashtbl_get m meta with
                        | Some d -> (
                            try float_of_string d with _ -> duration )
                        | None -> duration )
              in
              let length = Frame.video_of_seconds duration in
              let fade = fader length in
              let fadefun = fadefun () in
              state <- `Play (fade, fadefun, length, 0);
              (fade, fadefun, length, 0)
          | `Play (fade, fadefun, length, count) ->
              (fade, fadefun, length, count)
      in
      if Frame.is_partial ab then state <- `Idle;
      match video_content with
        | None -> ()
        | Some (rgb, off, len) ->
            let rgb = rgb.(0) in
            if count < length then
              for i = 0 to min (len - 1) (length - count - 1) do
                let m = fade (count + i) in
                fadefun (Video.get rgb (off + i)) m
              done;
            if state <> `Idle then
              state <- `Play (fade, fadefun, length, count + len)
  end

(** Fade-out after every frame. *)
class fade_out ~kind ?(meta = "liq_video_fade_out") duration fader fadefun
  source =
  object
    inherit operator ~name:"video.fade.out" kind [source]

    method stype = source#stype

    method abort_track = source#abort_track

    method self_sync = source#self_sync

    (* Fade-out length (in video frames) for the current track.
     * The value is set at the beginning of every track, depending on metadata. *)
    val mutable cur_length = None

    method remaining = source#remaining

    method is_ready = source#is_ready

    method private get_frame ab =
      let n = Frame.video_of_master source#remaining in
      let off_ticks = Frame.position ab in
      let video_content = VFrame.get_content ab source in
      (* In video frames: [length] of the fade. *)
      let fade, fadefun, length =
        match cur_length with
          | Some (f, g, l) -> (f, g, l)
          | None ->
              (* Set the length at the beginning of a track *)
              let duration =
                match Frame.get_metadata ab off_ticks with
                  | None -> duration
                  | Some m -> (
                      match Utils.hashtbl_get m meta with
                        | None -> duration
                        | Some d -> (
                            try float_of_string d with _ -> duration ) )
              in
              let l = Frame.video_of_seconds duration in
              let f = fader l in
              let g = fadefun () in
              cur_length <- Some (f, g, l);
              (f, g, l)
      in
      (* Reset the length at the end of a track *)
      if Frame.is_partial ab then cur_length <- None;

      (* Do the actual processing of video samples *)
      match video_content with
        | None -> ()
        | Some (rgb, off, len) -> (
            (* Process the buffer *)
              match if n >= 0 && n < length then Some n else None with
              | Some n ->
                  let rgb = rgb.(0) in
                  for i = 0 to len - 1 do
                    let m = fade (n - i) in
                    fadefun (Video.get rgb (off + i)) m
                  done
              | None -> () )
  end

(** Lang interface *)

let kind = Lang.kind_type_of_kind_format (Lang.any_fixed_with ~video:1 ())

(* TODO: share more with fade.ml *)
let proto =
  [
    ( "duration",
      Lang.float_t,
      Some (Lang.float 3.),
      Some
        "Duration of the fading. This value can be set on a per-file basis \
         using the metadata field passed as override." );
    ( "transition",
      Lang.string_t,
      Some (Lang.string "fade"),
      Some
        "Kind of transition \
         (fade|slide_left|slide_right|slide_up|slide_down|grow|disc|random)." );
    ( "type",
      Lang.string_t,
      Some (Lang.string "lin"),
      Some
        "Fader shape (lin|sin|log|exp): linear, sinusoidal, logarithmic or \
         exponential." );
    ("", Lang.source_t kind, None, None);
  ]

let rec transition_of_string p transition =
  let translate img x y =
    let tmp = Video.Image.copy img in
    Image.YUV420.fill_alpha img 0;
    Video.Image.add tmp ~x ~y img
  in
  let ifm n a = int_of_float (float_of_int n *. a) in
  match transition with
    | "fade" -> fun () img t -> Video.Image.fill_alpha img (ifm 256 t)
    | "slide_left" ->
        fun () buf t ->
          translate buf (ifm (Lazy.force Frame.video_width) (t -. 1.)) 0
    | "slide_right" ->
        fun () buf t ->
          translate buf (ifm (Lazy.force Frame.video_width) (1. -. t)) 0
    | "slide_up" ->
        fun () buf t ->
          translate buf 0 (ifm (Lazy.force Frame.video_height) (1. -. t))
    | "slide_down" ->
        fun () buf t ->
          translate buf 0 (ifm (Lazy.force Frame.video_height) (t -. 1.))
    | "grow" ->
        fun () img t ->
          let w = Video.Image.width img in
          let h = Video.Image.height img in
          let w' = ifm w t in
          let h' = ifm h t in
          let tmp = Video.Image.create w' h' in
          Video.Image.scale img tmp;
          Video.Image.fill_alpha img 0;
          let x = (w - w') / 2 in
          let y = (h - h') / 2 in
          Video.Image.add ~x ~y tmp img
    | "disc" ->
        let w = Lazy.force Frame.video_width in
        let h = Lazy.force Frame.video_height in
        let r_max =
          int_of_float (sqrt (float_of_int ((w * w) + (h * h)))) / 2
        in
        fun () buf t ->
          Image.YUV420.disk_alpha buf (w / 2) (h / 2) (ifm r_max t)
    | "random" ->
        let trans =
          [|
            "slide_left";
            "slide_right";
            "slide_up";
            "slide_down";
            "fade";
            "grow";
            "disc";
          |]
        in
        fun () ->
          let f =
            transition_of_string p trans.(Random.int (Array.length trans)) ()
          in
          f
    | _ ->
        raise
          (Lang_errors.Invalid_value
             (List.assoc "transition" p, "Invalid transition kind"))

let extract p =
  ( Lang.to_float (List.assoc "duration" p),
    (let mode = List.assoc "type" p in
     let f =
       (* A few typical shapes..
        * In theory, any mapping from [0:1] to [0:1] is OK,
        * preferably monotonic and one-to-one. *)
       match Lang.to_string mode with
         | "lin" -> fun x -> x
         | "log" ->
             let curve = 10. in
             let m = log (1. +. curve) in
             fun x -> log (1. +. (x *. 10.)) /. m
         | "exp" ->
             let curve = 2. in
             let m = exp curve -. 1. in
             fun x -> (exp (curve *. x) -. 1.) /. m
         | "sin" ->
             let pi = acos (-1.) in
             fun x -> (1. +. sin ((x -. 0.5) *. pi)) /. 2.
         | _ ->
             let msg =
               "The 'type' parameter should be 'lin','sin','log' or 'exp'!"
             in
             raise (Lang_errors.Invalid_value (mode, msg))
     in
     fun l ->
       let l = float l in
       fun i ->
         let i = float i /. l in
         f (max 0. (min 1. i))),
    (let transition = Lang.to_string (List.assoc "transition" p) in
     transition_of_string p transition),
    Lang.to_source (List.assoc "" p) )

let override_doc =
  Some
    "Metadata field which, if present and containing a float, overrides the \
     'duration' parameter for current track."

let () =
  Lang.add_operator "video.fade.in"
    ( ( "override",
        Lang.string_t,
        Some (Lang.string "liq_video_fade_in"),
        override_doc )
    :: proto )
    ~kind:(Lang.Unconstrained kind) ~category:Lang.VideoProcessing
    ~descr:
      "Fade the beginning of tracks. Metadata 'liq_video_fade_in' can be used \
       to set the duration for a specific track (float in seconds)."
    (fun p kind ->
      let d, f, t, s = extract p in
      let meta = Lang.to_string (List.assoc "override" p) in
      new fade_in ~kind ~meta d f t s);
  Lang.add_operator "video.fade.out"
    ( ( "override",
        Lang.string_t,
        Some (Lang.string "liq_video_fade_out"),
        override_doc )
    :: proto )
    ~kind:(Lang.Unconstrained kind) ~category:Lang.VideoProcessing
    ~descr:
      "Fade the end of tracks. Metadata 'liq_video_fade_out' can be used to \
       set the duration for a specific track (float in seconds)."
    (fun p kind ->
      let d, f, t, s = extract p in
      let meta = Lang.to_string (List.assoc "override" p) in
      new fade_out ~kind ~meta d f t s)
