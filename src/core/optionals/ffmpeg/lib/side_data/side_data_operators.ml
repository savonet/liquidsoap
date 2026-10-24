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

module P = Avcodec.Packet_side_data
module F = Avutil.Frame_side_data

let log = Log.make ["ffmpeg"; "side_data"]
let autorotate = Lang.add_module ~base:Lang_modules.ffmpeg "autorotate"
let faults = Faults.create log

(* §4. *)
let question_t =
  Lang.record_t
    [
      ( "cropping",
        Lang.nullable_t
          (Lang.record_t
             [
               ("bottom", Lang.int_t);
               ("left", Lang.int_t);
               ("right", Lang.int_t);
               ("top", Lang.int_t);
             ]) );
      ("reason", Lang.string_t);
      ("rotation", Lang.nullable_t Lang.float_t);
    ]

let filters_t =
  Lang.nullable_t
    (Lang.list_t
       (Lang.product_t Lang.string_t
          (Lang.list_t (Lang.product_t Lang.string_t Lang.string_t))))

let question (undecided : Avfilter.Utils.undecided_frame) =
  let reason, rotation, cropping =
    match undecided with
      | `Odd_rotation angle -> ("odd_rotation", Lang.float angle, Lang.null)
      | `Invalid_cropping { Avutil.top; bottom; left; right } ->
          ( "invalid_cropping",
            Lang.null,
            Lang.record
              [
                ("bottom", Lang.int bottom);
                ("left", Lang.int left);
                ("right", Lang.int right);
                ("top", Lang.int top);
              ] )
      | `Hardware_frame -> ("hardware_frame", Lang.null, Lang.null)
  in
  ( reason,
    Lang.record
      [
        ("cropping", cropping);
        ("reason", Lang.string reason);
        ("rotation", rotation);
      ] )

let answer value =
  Option.map
    (List.map (fun filter ->
         let name, options = Lang.to_product filter in
         ( Lang.to_string name,
           List.map
             (fun option ->
               let key, v = Lang.to_product option in
               `Pair (Lang.to_string key, `String (Lang.to_string v)))
             (Lang.to_list options) )))
    (Option.map Lang.to_list (Lang.to_option value))

(* §3.4: a raising handler decides nothing; the stream decodes as stored. *)
let handler fn undecided =
  let reason, q = question undecided in
  try answer (Lang.apply fn [("", q)])
  with exn ->
    Faults.uncounted faults ~kind:"autorotate handler"
      (Printf.sprintf
         "Autorotate handler failed on %s, video left as stored: %s" reason
         (Printexc.to_string exn));
    None

let () =
  ignore
    (Lang.add_builtin ~base:autorotate "on_undecided" ~category:`Liquidsoap
       ~descr:
         "Install the function deciding how to show video whose display matrix \
          or cropping the decoders cannot apply by themselves: a rotation that \
          is not a quarter turn (clockwise angle, in degrees), an invalid \
          cropping, or a hardware frame. It returns a list of filters `(name, \
          [(option, value)])` the frames go through, or `null` to leave the \
          video as stored, display matrix included. It applies to video \
          decoded from then on."
       [("", Lang.fun_t [(false, "", question_t)] filters_t, None, None)]
       Lang.unit_t
       (fun p ->
         Display.on_undecided := Some (handler (List.assoc "" p));
         Lang.unit))

(* §5 and §6, shared. *)
let rotation_of getter () =
  match Lang.to_valued_option Lang.to_float (getter ()) with
    | Some r when Float.is_finite r -> Some r
    | Some r ->
        Faults.uncounted faults ~kind:"rotation"
          (Printf.sprintf "Rotation %f ignored: not a finite number" r);
        None
    | None -> None

let kinds_of ~names ~all ~value remove =
  List.map
    (fun name ->
      match List.find_opt (fun kind -> names kind = name) all with
        | Some kind -> kind
        | None ->
            raise
              (Error.Invalid_value
                 (value, Printf.sprintf "Unknown side data kind: %s" name, [])))
    remove

let entries_value entries =
  Lang.list
    (List.map
       (fun (kind, data) ->
         Lang.record [("data", Lang.string data); ("kind", Lang.string kind)])
       entries)

let rotation_value = function
  | Some matrix -> (
      match Avutil.Display_matrix.rotation matrix with
        | Some r -> Lang.float r
        | None -> Lang.null)
  | None -> Lang.null

let meth ~entries ~matrix =
  [
    {
      Lang.name = "rotation";
      scheme = ([], Lang.fun_t [] (Lang.nullable_t Lang.float_t));
      descr =
        "Rotation of the current source's display matrix, in degrees \
         counter-clockwise.";
      value = (fun s -> Lang.val_fun [] (fun _ -> rotation_value (matrix s)));
    };
    {
      Lang.name = "side_data";
      scheme =
        ( [],
          Lang.fun_t []
            (Lang.list_t
               (Lang.record_t
                  [("data", Lang.string_t); ("kind", Lang.string_t)])) );
      descr = "Stream-wide side data of the current source.";
      value = (fun s -> Lang.val_fun [] (fun _ -> entries_value (entries s)));
    };
  ]

let infallible_note =
  " With an infallible input, a persistent data fault is a hard error: use \
   `mksafe` or `fallback` to keep streaming through it."

(* §5. *)
class copy_side_data ~field ~rotation ~remove (source : Source.source) =
  object (self)
    inherit Source.operator ~name:"track.ffmpeg.side_data" [source]
    val mutable last_source = Source_id.none
    val mutable cache = None
    val mutable stored = []
    val mutable rejected_until = None
    method fallible = source#fallible
    method remaining = source#remaining
    method effective_source = source#effective_source
    method abort_track = source#abort_track
    method private self_sync = source#cached_self_sync
    method! seek len = source#seek len

    method private can_generate_frame =
      (match rejected_until with
        | Some t when Unix.gettimeofday () < t -> false
        | Some _ ->
            rejected_until <- None;
            cache <- None;
            true
        | None -> true)
      && source#is_ready

    method stored = stored

    method private edit ~r params =
      let entries =
        List.filter
          (fun (e : P.raw) -> not (List.mem e.kind remove))
          (Copy_params.side_data params)
      in
      let entries =
        match (r, params.Copy_params.media) with
          | Some r, `Video _ ->
              List.filter (fun (e : P.raw) -> e.kind <> `Displaymatrix) entries
              @ [P.encode (`Display_matrix (Avutil.Display_matrix.make r))]
          | _ -> entries
      in
      if entries = Copy_params.side_data params then params
      else Copy_params.with_side_data params entries

    method private output_params ~r params =
      match cache with
        | Some (input, r', result) when input == params && r' = r -> result
        | _ ->
            let result = self#edit ~r params in
            let format = Frame.Fields.find field self#content_type in
            (try
               Content.merge format
                 (Ffmpeg_content.Copy.lift_params (Some result))
             with exn ->
               self#reject
                 (Printf.sprintf "%s\nOutput: %s\nNew source: %s"
                    (Printexc.to_string exn)
                    (match Ffmpeg_content.Copy.get_params format with
                      | Some p -> Copy_params.to_string p
                      | None -> "unknown")
                    (Copy_params.to_string result)));
            cache <- Some (params, r, result);
            result

    method private reject message =
      self#log#severe "Source rejected: %s" message;
      if not source#fallible then
        Runtime_error.raise ~pos:(Option.to_list self#pos)
          ~message:
            (Printf.sprintf "%s cannot pass %s: %s" self#id source#id message)
          "ffmpeg"
      else (
        rejected_until <- Some (Unix.gettimeofday () +. Faults.fault_retry_delay);
        source#abort_track;
        raise Exit)

    method private packet packet =
      let edit (p : _ Avcodec.Packet.t) wrap =
        if
          List.exists
            (fun (e : P.raw) -> List.mem e.kind remove)
            (Avcodec.Packet.raw_side_data p)
        then (
          let p = Avcodec.Packet.dup p in
          List.iter (Avcodec.Packet.remove_side_data p) remove;
          wrap p)
        else packet
      in
      match packet with
        | `Video p -> edit p (fun p -> `Video p)
        | `Audio p -> edit p (fun p -> `Audio p)
        | `Subtitle p -> edit p (fun p -> `Subtitle p)

    method private generate_frame =
      let r = rotation () in
      let frame = source#get_frame in
      let data = Ffmpeg_content.Copy.get_data (Frame.get frame field) in
      try
        let runs =
          List.map
            (fun (start, (run : Ffmpeg_content.Copy.Data.run)) ->
              if run.source_id <> last_source then (
                last_source <- run.source_id;
                stored <- []);
              let source_params =
                Option.map (self#output_params ~r) run.source_params
              in
              Option.iter
                (fun p -> stored <- Copy_params.side_data p)
                source_params;
              ( start,
                Ffmpeg_content.Copy.Data.run ~continues:run.continues
                  ?source_params ~source_id:run.source_id
                  ~time_base:run.time_base ~length:run.length
                  (List.map
                     (fun (pos, packet) -> (pos, self#packet packet))
                     run.items) ))
            data.runs
        in
        let params =
          Ffmpeg_content.Copy.get_params
            (Frame.Fields.find field self#content_type)
        in
        source#set_frame_data field
          (fun ?offset:_ ?length:_ () ->
            Ffmpeg_content.Copy.lift_data
              { Ffmpeg_content.Copy.Data.params; length = data.length; runs })
          ()
      with Exit -> Frame.add_track_mark (Frame.slice frame 0) 0

    initializer
      self#on_wake_up (fun () ->
          rejected_until <- None;
          cache <- None;
          stored <- [];
          last_source <- Source_id.none)
  end

(* §6. *)
class raw_side_data ~field ~rotation ~remove (source : Source.source) =
  object (self)
    inherit Source.operator ~name:"track.ffmpeg.raw.side_data" [source]
    val mutable last_source = Source_id.none
    val mutable last_frame = None
    method fallible = source#fallible
    method remaining = source#remaining
    method effective_source = source#effective_source
    method abort_track = source#abort_track
    method private self_sync = source#cached_self_sync
    method! seek len = source#seek len
    method private can_generate_frame = source#is_ready

    method global =
      match last_frame with
        | None -> []
        | Some frame ->
            List.filter
              (fun (e : F.raw) -> List.mem `Global (F.props e.kind))
              (Avutil.Frame.side_data frame)

    method private edit r frame =
      let frame = Avutil.Frame.dup frame in
      List.iter (Avutil.Frame.remove_side_data frame) remove;
      Option.iter
        (fun r ->
          Avutil.Frame.remove_side_data frame `Displaymatrix;
          Avutil.Frame.add_side_data frame
            (F.encode (`Display_matrix (Avutil.Display_matrix.make r))))
        r;
      frame

    method private generate_frame =
      let r = rotation () in
      let frame = source#get_frame in
      let data = Ffmpeg_content.Video.get_data (Frame.get frame field) in
      let unchanged = r = None && remove = [] in
      let runs =
        List.map
          (fun (start, (run : Ffmpeg_content.Video.Data.run)) ->
            if run.source_id <> last_source then (
              last_source <- run.source_id;
              last_frame <- None);
            let items =
              List.map
                (fun (pos, f) ->
                  let f = if unchanged then f else self#edit r f in
                  last_frame <- Some f;
                  (pos, f))
                run.items
            in
            ( start,
              Ffmpeg_content.Video.Data.run ~continues:run.continues
                ~source_id:run.source_id ~time_base:run.time_base
                ~length:run.length items ))
          data.runs
      in
      if unchanged then frame
      else
        Frame.set frame field
          (Ffmpeg_content.Video.lift_data
             {
               Ffmpeg_content.Video.Data.params = data.params;
               length = data.length;
               runs;
             })
  end

let kind_t kind = Type.make (Format_type.descr (`Kind kind))

let args ~family =
  [
    ( "rotation",
      Lang.getter_t (Lang.nullable_t Lang.float_t),
      Some Lang.null,
      Some
        "A non-null value sets the stream's display rotation, in degrees \
         counter-clockwise." );
    ( "remove",
      Lang.list_t Lang.string_t,
      Some (Lang.list []),
      Some
        (Printf.sprintf
           "Kinds of side data to remove, by FFmpeg's %s side-data name." family)
    );
  ]

let parse p ~names ~all =
  let remove_value = List.assoc "remove" p in
  let remove =
    kinds_of ~names ~all ~value:remove_value
      (List.map Lang.to_string (Lang.to_list remove_value))
  in
  let rotation = rotation_of (Lang.to_getter (List.assoc "rotation" p)) in
  let field, source = Lang.to_track (List.assoc "" p) in
  (field, source, rotation, remove)

let () =
  let input_t = kind_t Ffmpeg_content.Copy.kind in
  ignore
    (Lang.add_track_operator ~base:Lang_modules.track_ffmpeg "side_data"
       ~category:`Track
       ~descr:
         ("Edit the side data of a copied track: set its display rotation, \
           remove kinds of side data." ^ infallible_note)
       ~meth:
         (meth
            ~entries:(fun (s : copy_side_data) ->
              List.map (fun (e : P.raw) -> (P.name e.kind, e.data)) s#stored)
            ~matrix:(fun s -> P.display_matrix s#stored))
       (args ~family:"packet" @ [("", input_t, None, None)])
       ~return_t:(kind_t Ffmpeg_content.Copy.kind)
       (fun p ->
         let field, source, rotation, remove =
           parse p ~names:P.name ~all:P.kinds
         in
         (field, new copy_side_data ~field ~rotation ~remove source)));
  let raw_t = kind_t Ffmpeg_content.Video.kind in
  ignore
    (Lang.add_track_operator ~base:Lang_modules.track_ffmpeg_raw "side_data"
       ~category:`Track
       ~descr:
         ("Edit the side data of a raw video track: set its display rotation, \
           remove kinds of side data." ^ infallible_note)
       ~meth:
         (meth
            ~entries:(fun (s : raw_side_data) ->
              List.map (fun (e : F.raw) -> (F.name e.kind, e.data)) s#global)
            ~matrix:(fun s -> F.display_matrix s#global))
       (args ~family:"frame" @ [("", raw_t, None, None)])
       ~return_t:raw_t
       (fun p ->
         let field, source, rotation, remove =
           parse p ~names:F.name ~all:F.kinds
         in
         (field, new raw_side_data ~field ~rotation ~remove source)))
