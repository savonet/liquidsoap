(*****************************************************************************
  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

(** Play multiple sources at the same time, and perform weighted mix *)

let max_remaining a b = if b = -1 || a = -1 then -1 else max a b

type 'a field = { position : int; weight : 'a; field : Frame.field }
type 'a track = { mutable fields : 'a field list; source : Source.source }

class virtual base ~name tracks =
  let sources = List.map (fun { source } -> source) tracks in
  let self_sync_type = Utils.self_sync_type sources in
  object (self)
    inherit Source.operator ~name sources as super

    method stype =
      if List.exists (fun s -> s#stype = `Infallible) sources then `Infallible
      else `Fallible

    method self_sync =
      (Lazy.force self_sync_type, List.exists (fun s -> snd s#self_sync) sources)

    method remaining =
      let f cur pos =
        match (cur, pos) with
          | -1, -1 -> -1
          | x, -1 | -1, x -> x
          | x, y -> max_remaining x y
      in
      List.fold_left f (-1)
        (List.map
           (fun s -> s#remaining)
           (List.filter (fun s -> s#is_ready) sources))

    method abort_track = List.iter (fun s -> s#abort_track) sources
    method is_ready = List.exists (fun s -> s#is_ready) sources
    method seek n = match sources with [s] -> s#seek n | _ -> 0
    val mutable track_frames = Hashtbl.create (List.length tracks)

    method private track_frame source =
      try Hashtbl.find track_frames source
      with Not_found ->
        let f = Frame.create source#content_type in
        Hashtbl.add track_frames source f;
        f

    method private feed_track ~offset pos { source } =
      let tmp = self#track_frame source in
      let start = Frame.position tmp in
      let tmp_pos =
        if start <= offset then (
          source#get tmp;
          Frame.position tmp)
        else start
      in
      min pos tmp_pos

    method private feed ~offset tracks =
      List.fold_left (self#feed_track ~offset) max_int tracks

    (* For backward compatibility: set metadata from the first
       track effectively summed. This should be called after #feed *)
    method private set_metadata buf offset position =
      match
        List.fold_left
          (fun cur { fields; source } ->
            if not source#is_ready then cur
            else
              List.fold_left
                (fun cur { position; _ } ->
                  match cur with
                    | None -> Some (source, position)
                    | Some (_, pos) when position < pos ->
                        Some (source, position)
                    | _ -> cur)
                cur fields)
          None tracks
      with
        | None -> ()
        | Some (source, _) ->
            let tmp = self#track_frame source in
            List.iter
              (fun (pos, m) ->
                if offset <= pos && pos <= position then
                  Frame.set_metadata buf pos m)
              (Frame.get_all_metadata tmp)

    method! advance =
      super#advance;
      Hashtbl.iter (fun _ frame -> Frame.clear frame) track_frames
  end

(** Add/mix several sources together.
  * If [renorm], renormalize the PCM channels. *)
class audio_add ~renorm ~power ~field tracks =
  object (self)
    inherit base ~name:"audio.add" tracks

    method private get_frame buf =
      let renorm = renorm () in
      let power = power () in
      let total_weight, tracks =
        List.fold_left
          (fun (total_weight, tracks) { source; fields } ->
            let source_weight, fields =
              List.fold_left
                (fun (source_weight, fields) field ->
                  let weight = field.weight () in
                  let weight = if power then weight *. weight else weight in
                  (source_weight +. weight, { field with weight } :: fields))
                (0., []) fields
            in
            ( total_weight +. source_weight,
              if source#is_ready then { source; fields } :: tracks else tracks
            ))
          (0., []) tracks
      in
      let total_weight = if power then sqrt total_weight else total_weight in
      let offset = Frame.position buf in
      let pos = self#feed ~offset tracks in
      assert (offset <= pos);
      let audio_offset = Frame.audio_of_main offset in
      let audio_len = Frame.audio_of_main (pos - offset) in
      let pcm = Content.Audio.get_data (Frame.get buf field) in
      Audio.clear pcm audio_offset audio_len;
      List.iter
        (fun { source; fields } ->
          let tmp = self#track_frame source in
          List.iter
            (fun { field; weight } ->
              let track_pcm = Content.Audio.get_data (Frame.get tmp field) in
              let c = if renorm then weight /. total_weight else weight in
              if c <> 1. then Audio.amplify c track_pcm audio_offset audio_len;
              Audio.add pcm audio_offset track_pcm audio_offset audio_len)
            fields)
        tracks;
      Frame.add_break buf pos;
      self#set_metadata buf offset pos
  end

class video_add ~field ~add tracks =
  object (self)
    inherit base ~name:"video.add" tracks

    method private get_frame buf =
      let tracks =
        List.fold_left
          (fun tracks track ->
            if track.source#is_ready then track :: tracks else tracks)
          [] tracks
      in
      let offset = Frame.position buf in
      let pos = self#feed ~offset tracks in
      let vbuf = Content.Video.get_data (Frame.get buf field) in
      let ( ! ) = Frame.video_of_main in
      List.iter
        (fun { source; fields } ->
          let tmp = self#track_frame source in
          List.iter
            (fun { position; field; _ } ->
              let vtmp = Content.Video.get_data (Frame.get tmp field) in
              for i = !offset to !pos - 1 do
                let img =
                  add position (Video.Canvas.get vtmp i)
                    (Video.Canvas.get vbuf i)
                in
                Video.Canvas.set vbuf i img
              done)
            fields)
        tracks;
      Frame.add_break buf pos;
      self#set_metadata buf offset pos
  end

let get_tracks ~mk_weight p =
  let track_values = Lang.to_list (List.assoc "" p) in
  List.fold_left
    (fun (tracks, position) v ->
      let field, s = Lang.to_track v in
      let weight = mk_weight v in
      let field = { position; weight; field } in
      let track =
        match List.find_opt (fun { source } -> s == source) tracks with
          | Some track ->
              track.fields <- track.fields @ [field];
              tracks
          | None -> tracks @ [{ source = s; fields = [field] }]
      in
      (track, position + 1))
    ([], 0) track_values

let _ =
  let frame_t = Format_type.audio () in
  let track_t =
    Type.meth ~optional:true "weight" ([], Lang.getter_t Lang.float_t) frame_t
  in
  Lang.add_track_operator ~base:Modules.track_audio "add" ~category:`Audio
    ~descr:"Mix audio tracks with optional normalization."
    [
      ( "normalize",
        Lang.getter_t Lang.bool_t,
        Some (Lang.bool true),
        Some
          "Divide by the sum of weights of ready sources (or by the number of \
           ready sources if weights are not specified)." );
      ( "power",
        Lang.getter_t Lang.bool_t,
        Some (Lang.bool false),
        Some "Perform constant-power normalization." );
      ("", Lang.list_t track_t, None, None);
    ]
    ~return_t:frame_t
    (fun p ->
      let tracks, _ =
        get_tracks
          ~mk_weight:(fun v ->
            try Lang.to_float_getter (Liquidsoap_lang.Value.invoke v "weight")
            with _ -> fun () -> 1.)
          p
      in
      let renorm = Lang.to_bool_getter (List.assoc "normalize" p) in
      let power = List.assoc "power" p |> Lang.to_bool_getter in
      let field = Frame.Fields.audio in
      (field, new audio_add ~renorm ~power ~field tracks))

let _ =
  let frame_t = Format_type.video () in
  Lang.add_track_operator ~base:Modules.track_video "add" ~category:`Video
    ~descr:"Merge video tracks."
    [("", Lang.list_t frame_t, None, None)]
    ~return_t:frame_t
    (fun p ->
      let tracks, _ = get_tracks ~mk_weight:(fun _ -> ()) p in
      let add _ = Video.Canvas.Image.add in
      let field = Frame.Fields.video in
      (field, new video_add ~add ~field tracks))

let tile_pos n =
  let vert l x y x' y' =
    if l = 0 then [||]
    else (
      let dx = (x' - x) / l in
      let x = ref (x - dx) in
      Array.init l (fun _ ->
          x := !x + dx;
          (!x, y, dx, y' - y)))
  in
  let x' = Lazy.force Frame.video_width in
  let y' = Lazy.force Frame.video_height in
  let horiz m n =
    Array.append (vert m 0 0 x' (y' / 2)) (vert n 0 (y' / 2) x' y')
  in
  horiz (n / 2) (n - (n / 2))

let _ =
  let frame_t = Format_type.video () in
  Lang.add_track_operator ~base:Modules.track_video "tile" ~category:`Video
    ~descr:"Tile video tracks."
    [
      ( "proportional",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Scale preserving the proportions." );
      ("", Lang.list_t frame_t, None, None);
    ]
    ~return_t:frame_t
    (fun p ->
      let tracks, total_tracks = get_tracks ~mk_weight:(fun _ -> ()) p in
      let proportional = Lang.to_bool (List.assoc "proportional" p) in
      let tp = tile_pos total_tracks in
      let scale = Video_converter.scaler () in
      let add n tmp buf =
        let x, y, w, h = tp.(n) in
        let x, y, w, h =
          if proportional then (
            let sw, sh =
              (Video.Canvas.Image.width buf, Video.Canvas.Image.height buf)
            in
            if w * sh < sw * h then (
              let h' = sh * w / sw in
              (x, y + ((h - h') / 2), w, h'))
            else (
              let w' = sw * h / sh in
              (x + ((w - w') / 2), y, w', h)))
          else (x, y, w, h)
        in
        let tmp = Video.Canvas.Image.render tmp in
        let tmp' = Video.Image.create w h in
        scale tmp tmp';
        let tmp' =
          Video.Canvas.Image.make ~x ~y ~width:(-1) ~height:(-1) tmp'
        in
        Video.Canvas.Image.add tmp' buf
      in
      let field = Frame.Fields.video in
      (field, new video_add ~field ~add tracks))
