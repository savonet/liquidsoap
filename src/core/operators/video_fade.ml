(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

open Mm
open Source

let video_fade = Lang.add_module ~base:Modules.video "fade"

(** Fade-in at the beginning of every track. The [duration] is in seconds. *)
class fade_in ?(meta = "liq_video_fade_in") duration fader fadefun source =
  object (self)
    inherit operator ~name:"video.fade.in" [source]
    method fallible = source#fallible
    method private can_generate_frame = source#is_ready
    method abort_track = source#abort_track
    method remaining = source#remaining
    method self_sync = source#self_sync
    method effective_source = source#effective_source
    val mutable state = `Idle

    method private process frame =
      let fade, fadefun, duration, position =
        match state with
          | `Idle ->
              let duration =
                match Frame.get_metadata frame 0 with
                  | None -> duration
                  | Some m -> (
                      match Frame.Metadata.find_opt meta m with
                        | Some d -> (
                            try float_of_string d with _ -> duration)
                        | None -> duration)
              in
              let fade = fader (Frame.video_of_seconds duration) in
              let duration = Frame.main_of_seconds duration in
              let fadefun = fadefun () in
              let v = (fade, fadefun, duration, 0) in
              state <- `Play v;
              v
          | `Play v -> v
      in
      if position < duration then (
        let buf =
          Content.Video.get_data
            (Content.copy (Frame.get frame Frame.Fields.video))
        in
        let data =
          List.mapi
            (fun i (pos, img) ->
              let m = fade (Frame.video_of_main position + i) in
              ignore (fadefun img m);
              (pos, img))
            buf.Content.Video.data
        in
        state <- `Play (fade, fadefun, duration, position + Frame.position frame);
        Frame.set_data frame Frame.Fields.video Content.Video.lift_data
          { buf with Content.Video.data })
      else frame

    method private generate_frame =
      match self#split_frame source#get_frame with
        | frame, None -> self#process frame
        | end_track, Some begin_track ->
            let end_track = self#process end_track in
            state <- `Idle;
            Frame.append end_track (self#process begin_track)
  end

(** Fade-out after every frame. *)
class fade_out ?(meta = "liq_video_fade_out") duration fader fadefun source =
  object (self)
    inherit operator ~name:"video.fade.out" [source]
    method fallible = source#fallible
    method abort_track = source#abort_track
    method self_sync = source#self_sync
    method effective_source = source#effective_source

    (* Fade-out length (in video frames) for the current track.
     * The value is set at the beginning of every track, depending on metadata. *)
    val mutable cur_length = None
    method remaining = source#remaining
    method private can_generate_frame = source#is_ready

    method private process_frame ~remaining frame =
      (* In main ticks: [length] of the fade. *)
      let fade, fadefun, length =
        match cur_length with
          | Some (f, g, l) -> (f, g, l)
          | None ->
              (* Set the length at the beginning of a track *)
              let duration =
                match Frame.get_metadata frame 0 with
                  | None -> duration
                  | Some m -> (
                      match Frame.Metadata.find_opt meta m with
                        | None -> duration
                        | Some d -> (
                            try float_of_string d with _ -> duration))
              in
              let l = Frame.main_of_seconds duration in
              let f = fader (Frame.video_of_seconds duration) in
              let g = fadefun () in
              cur_length <- Some (f, g, l);
              (f, g, l)
      in

      if remaining < length then (
        let content = Content.copy (Frame.get frame Frame.Fields.video) in
        let buf = Content.Video.get_data content in

        let data =
          List.mapi
            (fun i (pos, img) ->
              let m = fade (Frame.video_of_main remaining - i) in
              (* TODO @smimram *)
              ignore (fadefun img m);
              (pos, img))
            buf.Content.Video.data
        in

        Frame.set_data frame Frame.Fields.video Content.Video.lift_data
          { buf with Content.Video.data })
      else frame

    method private generate_frame =
      match self#split_frame source#get_frame with
        | frame, None -> self#process_frame ~remaining:source#remaining frame
        | end_track, Some begin_track ->
            let end_track = self#process_frame ~remaining:0 end_track in
            cur_length <- None;
            Frame.append end_track
              (self#process_frame ~remaining:source#remaining begin_track)
  end

(** Lang interface *)

(* TODO: share more with fade.ml *)
let proto frame_t =
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
    ("", Lang.source_t frame_t, None, None);
  ]

let rec transition_of_string p transition =
  let translate img dx dy = Video.Canvas.Image.translate dx dy img in
  let ifm n a = int_of_float (float_of_int n *. a) in
  match transition with
    | "fade" ->
        fun () img t ->
          Video.Canvas.Image.iter
            (fun img -> Image.YUV420.fill_alpha img (ifm 256 t))
            img
    | "slide_left" ->
        fun () buf t ->
          translate buf (ifm (Video.Canvas.Image.width buf) (t -. 1.)) 0
    | "slide_right" ->
        fun () buf t ->
          translate buf (ifm (Video.Canvas.Image.width buf) (1. -. t)) 0
    | "slide_up" ->
        fun () buf t ->
          translate buf 0 (ifm (Video.Canvas.Image.height buf) (1. -. t))
    | "slide_down" ->
        fun () buf t ->
          translate buf 0 (ifm (Video.Canvas.Image.height buf) (t -. 1.))
    | "grow" ->
        fun () img t ->
          let w = Video.Canvas.Image.width img in
          let h = Video.Canvas.Image.height img in
          let w' = ifm w t in
          let h' = ifm h t in
          let img = Video.Canvas.Image.render img in
          let out = Video.Image.create w' h' in
          Image.YUV420.scale img out;
          let x = (w - w') / 2 in
          let y = (h - h') / 2 in
          Video.Canvas.Image.create w h |> Video.Canvas.Image.translate x y
    | "disc" ->
        fun () buf t ->
          let w = Video.Canvas.Image.width buf in
          let h = Video.Canvas.Image.height buf in
          let r_max =
            int_of_float (sqrt (float_of_int ((w * w) + (h * h)))) / 2
          in
          Video.Canvas.Image.iter
            (fun buf ->
              Image.YUV420.disk_alpha buf (w / 2) (h / 2) (ifm r_max t))
            buf
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
          (Error.Invalid_value
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
             raise (Error.Invalid_value (mode, msg))
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

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~video:(Format_type.video ()) ())
  in
  Lang.add_operator ~base:video_fade "in"
    (( "override",
       Lang.string_t,
       Some (Lang.string "liq_video_fade_in"),
       override_doc )
    :: proto frame_t)
    ~return_t:frame_t ~category:`Video
    ~descr:
      "Fade the beginning of tracks. Metadata 'liq_video_fade_in' can be used \
       to set the duration for a specific track (float in seconds)."
    (fun p ->
      let d, f, t, s = extract p in
      let meta = Lang.to_string (List.assoc "override" p) in
      new fade_in ~meta d f t s)

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~video:(Format_type.video ()) ())
  in
  Lang.add_operator ~base:video_fade "out"
    (( "override",
       Lang.string_t,
       Some (Lang.string "liq_video_fade_out"),
       override_doc )
    :: proto frame_t)
    ~return_t:frame_t ~category:`Video
    ~descr:
      "Fade the end of tracks. Metadata 'liq_video_fade_out' can be used to \
       set the duration for a specific track (float in seconds)."
    (fun p ->
      let d, f, t, s = extract p in
      let meta = Lang.to_string (List.assoc "override" p) in
      new fade_out ~meta d f t s)
