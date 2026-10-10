(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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
module Packet_side_data = Avcodec.Packet_side_data
module Frame_side_data = Avutil.Frame_side_data

type edit = { rotation : unit -> float option; remove : string list }

let no_edit { rotation; remove } = rotation () = None && remove = []

let remove_entries { remove; _ } entries =
  List.filter
    (fun { Packet_side_data.kind; _ } ->
      not (List.mem (Packet_side_data.name kind) remove))
    entries

let rotate_entries { rotation; _ } entries =
  match rotation () with
    | None -> entries
    | Some angle ->
        List.filter
          (fun { Packet_side_data.kind; _ } -> kind <> `Displaymatrix)
          entries
        @ [
            Packet_side_data.encode
              (`Display_matrix (Avutil.Display_matrix.make angle));
          ]

type 'params edited = { entries : Packet_side_data.raw list; params : 'params }

(* [params] is returned as it came when nothing changes: encoders tell a new
   stream by its parameters. *)
let edit_params ~edit_entries params =
  let entries = Avcodec.params_side_data params in
  let edited = edit_entries entries in
  {
    entries = edited;
    params =
      (if edited = entries then params
       else Avcodec.params_with_side_data params edited);
  }

(* A display matrix means something on video only. *)
let edit_copy_params edit (params : Ffmpeg_copy_content.codec_params) =
  let removed = remove_entries edit in
  let rewrap ~codec_params ~wrap edited =
    {
      entries = edited.entries;
      params =
        (if edited.params == codec_params then params else wrap edited.params);
    }
  in
  match params with
    | `Audio codec_params ->
        rewrap ~codec_params
          ~wrap:(fun params -> `Audio params)
          (edit_params ~edit_entries:removed codec_params)
    | `Video ({ Ffmpeg_copy_content.codec_params; _ } as video) ->
        rewrap ~codec_params
          ~wrap:(fun codec_params -> `Video { video with codec_params })
          (edit_params
             ~edit_entries:(fun entries ->
               rotate_entries edit (removed entries))
             codec_params)
    | `Subtitle ({ Ffmpeg_copy_content.codec_params; _ } as subtitle) ->
        rewrap ~codec_params
          ~wrap:(fun codec_params -> `Subtitle { subtitle with codec_params })
          (edit_params ~edit_entries:removed codec_params)

type copy_edit = {
  source_params : Ffmpeg_copy_content.codec_params;
  source_rotation : float option;
  result : Ffmpeg_copy_content.codec_params edited;
}

class virtual passthrough ~name (source : source) =
  object
    inherit operator ~name [source]
    method fallible = source#fallible
    method private can_generate_frame = source#is_ready
    method remaining = source#remaining
    method abort_track = source#abort_track
    method effective_source = source#effective_source
    method private self_sync = source#cached_self_sync
    method virtual side_data : (string * string) list
    method virtual rotation : float option
  end

class copy ~field ~edit source =
  object (self)
    inherit passthrough ~name:"track.ffmpeg.side_data" source
    val mutable entries = []
    val mutable last_edit = None

    method side_data =
      List.map
        (fun { Packet_side_data.kind; data } ->
          (Packet_side_data.name kind, data))
        entries

    method rotation =
      Option.bind
        (Packet_side_data.display_matrix entries)
        Avutil.Display_matrix.rotation

    method private edit params =
      let rotation = edit.rotation () in
      match last_edit with
        | Some { source_params; source_rotation; result }
          when source_params == params && source_rotation = rotation ->
            result
        | _ ->
            let result = edit_copy_params edit params in
            last_edit <-
              Some
                { source_params = params; source_rotation = rotation; result };
            result

    method private generate_frame =
      let buf = source#get_frame in
      let content = Ffmpeg_copy_content.get_data (Frame.get buf field) in
      match Ffmpeg_content_base.params content with
        | None -> buf
        | Some params ->
            let edited = self#edit params in
            entries <- edited.entries;
            if edited.params == params then buf
            else
              Frame.set_data buf field Ffmpeg_copy_content.lift_data
                { content with params = Some edited.params }
  end

(* Frames are shared between the consumers of a track. *)
let edit_frame { rotation; remove } frame =
  let frame = Avutil.Frame.dup frame in
  List.iter
    (fun { Frame_side_data.kind; _ } ->
      if List.mem (Frame_side_data.name kind) remove then
        Avutil.Frame.remove_side_data frame kind)
    (Avutil.Frame.side_data frame);
  Option.iter
    (fun angle ->
      Avutil.Frame.add_side_data frame
        (Frame_side_data.encode
           (`Display_matrix (Avutil.Display_matrix.make angle))))
    (rotation ());
  frame

class raw ~field ~edit source =
  object (self)
    inherit passthrough ~name:"track.ffmpeg.raw.side_data" source
    val mutable last_frame = None

    method private entries =
      Option.fold ~none:[] ~some:Ffmpeg_utils.global_side_data last_frame

    method side_data =
      List.map
        (fun { Frame_side_data.kind; data } ->
          (Frame_side_data.name kind, data))
        self#entries

    method rotation =
      Option.bind
        (Frame_side_data.display_matrix self#entries)
        Avutil.Display_matrix.rotation

    method private generate_frame =
      let buf = source#get_frame in
      let content = Ffmpeg_raw_content.Video.get_data (Frame.get buf field) in
      let keep_last data =
        List.iter (fun (_, frame) -> last_frame <- Some frame) data
      in
      if no_edit edit then (
        List.iter
          (fun { Ffmpeg_content_base.data; _ } -> keep_last data)
          content.chunks;
        buf)
      else (
        let edit_chunk ({ Ffmpeg_content_base.data; _ } as chunk) =
          let data =
            List.map
              (fun (position, frame) -> (position, edit_frame edit frame))
              data
          in
          keep_last data;
          { chunk with data }
        in
        Frame.set_data buf field Ffmpeg_raw_content.Video.lift_data
          { content with chunks = List.map edit_chunk content.chunks })
  end

let entry_t = Lang.record_t [("kind", Lang.string_t); ("data", Lang.string_t)]

let proto frame_t =
  [
    ( "rotation",
      Lang.getter_t (Lang.nullable_t Lang.float_t),
      Some Lang.null,
      Some
        "Set the display rotation of the stream, in degrees counter-clockwise. \
         `null` leaves it as it is." );
    ( "remove",
      Lang.list_t Lang.string_t,
      Some (Lang.list []),
      Some "Kinds of side data to remove from the stream, by FFmpeg name." );
    ("", frame_t, None, None);
  ]

let meth () =
  [
    {
      Lang.name = "side_data";
      scheme = ([], Lang.fun_t [] (Lang.list_t entry_t));
      descr =
        "Stream-wide side data of the last data that went through, by FFmpeg \
         name. `data` is in FFmpeg's in-memory layout.";
      value =
        (fun (op :
               < side_data : (string * string) list
               ; rotation : float option
               ; .. >)
        ->
          Lang.val_fun [] (fun _ ->
              Lang.list
                (List.map
                   (fun (kind, data) ->
                     Lang.record
                       [("kind", Lang.string kind); ("data", Lang.string data)])
                   op#side_data)));
    };
    {
      Lang.name = "rotation";
      scheme = ([], Lang.fun_t [] (Lang.nullable_t Lang.float_t));
      descr =
        "Display rotation of the stream, in degrees counter-clockwise. `null` \
         when the stream has none.";
      value =
        (fun op ->
          Lang.val_fun [] (fun _ ->
              match op#rotation with
                | None -> Lang.null
                | Some angle -> Lang.float angle));
    };
  ]

let edit_of_params p =
  let rotation = Lang.to_getter (List.assoc "rotation" p) in
  {
    rotation = (fun () -> Lang.to_valued_option Lang.to_float (rotation ()));
    remove = List.map Lang.to_string (Lang.to_list (List.assoc "remove" p));
  }

let _ =
  let frame_t =
    Type.make (Format_type.descr (`Kind Ffmpeg_copy_content.kind))
  in
  Lang.add_track_operator ~base:Builtins_ffmpeg_base.track_ffmpeg "side_data"
    (proto frame_t) ~return_t:frame_t ~category:`Track
    ~descr:
      "Read and change the stream-wide side data of an encoded track, such as \
       its display rotation." ~meth:(meth ()) (fun p ->
      let field, source = Lang.to_track (List.assoc "" p) in
      (field, new copy ~field ~edit:(edit_of_params p) source))

let _ =
  let frame_t =
    Type.make (Format_type.descr (`Kind Ffmpeg_raw_content.Video.kind))
  in
  Lang.add_track_operator ~base:Builtins_ffmpeg_base.track_ffmpeg_raw
    "side_data" (proto frame_t) ~return_t:frame_t ~category:`Track
    ~descr:
      "Read and change the stream-wide side data of a raw video track, such as \
       its display rotation." ~meth:(meth ()) (fun p ->
      let field, source = Lang.to_track (List.assoc "" p) in
      (field, new raw ~field ~edit:(edit_of_params p) source))

let ffmpeg_autorotate =
  Lang.add_module ~base:Builtins_ffmpeg_base.ffmpeg "autorotate"

let filter_t =
  Lang.product_t Lang.string_t
    (Lang.list_t (Lang.product_t Lang.string_t Lang.string_t))

let cropping_t =
  Lang.record_t
    [
      ("top", Lang.int_t);
      ("bottom", Lang.int_t);
      ("left", Lang.int_t);
      ("right", Lang.int_t);
    ]

let undecided_t =
  Lang.record_t
    [
      ("reason", Lang.string_t);
      ("rotation", Lang.nullable_t Lang.float_t);
      ("cropping", Lang.nullable_t cropping_t);
    ]

let undecided_value (undecided : Avfilter.Utils.undecided_frame) =
  let reason, rotation, cropping =
    match undecided with
      | `Hardware_frame -> ("hardware_frame", Lang.null, Lang.null)
      | `Odd_rotation angle -> ("odd_rotation", Lang.float angle, Lang.null)
      | `Invalid_cropping { Avutil.top; bottom; left; right } ->
          ( "invalid_cropping",
            Lang.null,
            Lang.record
              [
                ("top", Lang.int top);
                ("bottom", Lang.int bottom);
                ("left", Lang.int left);
                ("right", Lang.int right);
              ] )
  in
  Lang.record
    [
      ("reason", Lang.string reason);
      ("rotation", rotation);
      ("cropping", cropping);
    ]

let filter_spec_of_value filter : Avfilter.Utils.filter_spec =
  let name, args = Lang.to_product filter in
  ( Lang.to_string name,
    List.map
      (fun arg ->
        let key, content = Lang.to_product arg in
        `Pair (Lang.to_string key, `String (Lang.to_string content)))
      (Lang.to_list args) )

let _ =
  Lang.add_builtin ~base:ffmpeg_autorotate "on_undecided" ~category:`Liquidsoap
    ~descr:
      "Decide how decoded video is shown upright when liquidsoap cannot decide \
       by itself. Without it, such video is left as stored and a warning is \
       logged. `reason` is one of: `\"odd_rotation\"`, when the video is \
       rotated by `rotation` degrees clockwise, which is not a quarter turn; \
       `\"invalid_cropping\"`, when `cropping` leaves no picture; \
       `\"hardware_frame\"`, when the frames are in a hardware pixel format. \
       Return the FFmpeg filters to apply, as `(name, [(option, value)])`, or \
       `null` to leave the video as stored. It applies to the decoders created \
       afterwards."
    [
      ( "",
        Lang.fun_t
          [(false, "", undecided_t)]
          (Lang.nullable_t (Lang.list_t filter_t)),
        None,
        None );
    ]
    Lang.unit_t
    (fun p ->
      let handler = List.assoc "" p in
      Ffmpeg_avfilter_utils.Display.set_on_undecided (fun undecided ->
          Lang.to_valued_option
            (fun filters ->
              List.map filter_spec_of_value (Lang.to_list filters))
            (Lang.apply handler [("", undecided_value undecided)]));
      Lang.unit)
