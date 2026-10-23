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

exception Aborted
exception Source_fault of string

let log = Log.make ["decoder"; "ffmpeg"]
let seek_preroll = 0.5

type kind = [ `Copy | `Decoded | `Data ]

(* One item read, with its timestamps converted to absolute main ticks. *)
type queued = {
  seq : int;
  item : Per_stream.item;
  raw_key : int option;
  key : int option;
  pts : int option;
  item_end : int option;
  key_packet : bool;
}

type entry = {
  stream : Stream_description.stream;
  kind : kind;
  per_stream : Per_stream.t;
  needs_key : bool;
  mutable origin : int;
  mutable last_raw_key : int option;
  mutable crossed : int;
  mutable threshold : int option;
  mutable held : queued list;
  mutable released : bool;
  mutable progress : int option;
  mutable latest_start : int option;
  mutable previous_key : int option;
  mutable skipped : int;
  queue : queued Queue.t;
}

type event = { number : int; shift : int }

type t = {
  container : Avutil.input Avutil.container;
  description : Stream_description.t;
  ctype : Frame.content_type;
  decode_first_metadata : bool;
  request_metadata : Frame.metadata;
  faults : Faults.t;
  container_origin : int;
  ts_discont : bool;
  mutable entries : entry list;
  mutable source_id : Source_id.t;
  mutable started : bool;
  mutable mapping_start : int option;
  mutable target : int option;
  mutable next_seq : int;
  mutable event : event option;
  mutable events : int;
  mutable had_discontinuity : bool;
  mutable early_sparse : (entry * queued) list;
  mutable reached : int;
  mutable sparse_reached : int;
  mutable window_warned : bool;
  mutable window_dropped : int;
  mutable buffer : Decoder.buffer option;
  mutable container_metadata : (string * string) list option;
  stream_metadata : (int, (string * string) list) Hashtbl.t;
  mutex : Mutex.t;
}

let main () = Time_base.main ()
let micro = { Avutil.num = 1; den = 1_000_000 }

let to_main ~src ts =
  Option.bind ts (Time_base.rescale_int64 ~src ~dst:(main ()))

let non_sparse t =
  List.filter (fun e -> not e.per_stream.Per_stream.sparse) t.entries

let field_name e =
  Frame.Fields.string_of_field e.stream.Stream_description.field

let output t field =
  let generator () =
    match t.buffer with
      | Some b -> b.Decoder.generator
      | None -> failwith "FFmpeg decoder used without a buffer"
  in
  {
    Per_stream.put = (fun data -> Generator.put (generator ()) field data);
    put_image =
      (fun ~fps image ->
        match t.buffer with
          | Some b -> b.Decoder.put_yuva420p ~field ~fps image
          | None -> failwith "FFmpeg decoder used without a buffer");
    add_metadata = (fun m -> Generator.add_metadata (generator ()) m);
  }

(* The row of decoder.md §6's table a stream maps to, from the requested
   format alone. *)
type choice =
  [ `Copy of Copy_params.t
  | `Timed_id3
  | `Internal_audio of Content_base.kind * int
  | `Internal_video of Content.format
  | `Raw_audio of Content.format
  | `Raw_video of Content.format
  | `Text_subtitles
  | `Bitmap_subtitles of Content.format ]

let unsupported (stream : Stream_description.stream) =
  failwith
    (Printf.sprintf "Field %s: no FFmpeg decoder for the requested content"
       (Frame.Fields.string_of_field stream.field))

let choose ~ctype (stream : Stream_description.stream) : choice option =
  match stream.media with
    | `Data p -> (
        match Avcodec.Unknown.get_params_id p with
          | `Timed_id3 -> Some `Timed_id3
          | _ -> None)
    | _ -> (
        match Frame.Fields.find_opt stream.field ctype with
          | None -> None
          | Some format when Ffmpeg_content.Copy.is_format format ->
              Option.map
                (fun p -> `Copy p)
                (Stream_description.copy_params stream)
          | Some format when Ffmpeg_content.Audio.is_format format -> (
              match stream.media with
                | `Audio _ -> Some (`Raw_audio format)
                | _ -> unsupported stream)
          | Some format when Ffmpeg_content.Video.is_format format -> (
              match stream.media with
                | `Video _ -> Some (`Raw_video format)
                | _ -> unsupported stream)
          | Some format -> (
              match stream.media with
                | `Audio _ when Content.Audio.is_format format ->
                    Some
                      (`Internal_audio
                         ( Content.kind format,
                           Content.Audio.channels_of_format format ))
                | `Audio _
                  when Content_pcm_s16.is_format format
                       || Content_pcm_f32.is_format format ->
                    Some
                      (`Internal_audio
                         ( Content.kind format,
                           Pcm_format.channels_of_format
                             ~get_params:Content_pcm_s16.get_params format ))
                | `Video _ when Content.Video.is_format format ->
                    Some (`Internal_video format)
                | `Subtitle _ when Subtitle_content.is_format format ->
                    Some `Text_subtitles
                | `Subtitle _ when Content.Video.is_format format ->
                    Some (`Bitmap_subtitles format)
                | _ -> unsupported stream))

let kind_of_choice : choice -> kind = function
  | `Copy _ -> `Copy
  | `Timed_id3 -> `Data
  | `Internal_audio _ | `Internal_video _ | `Raw_audio _ | `Raw_video _
  | `Text_subtitles | `Bitmap_subtitles _ ->
      `Decoded

let create_decoder ~output ~faults ~source_id
    (stream : Stream_description.stream) : choice -> Per_stream.t = function
  | `Copy params -> Copy_decoder.create ~output ~faults ~source_id stream params
  | `Timed_id3 -> Timed_id3_decoder.create ~output ~faults
  | `Internal_audio (pcm_kind, channels) -> (
      match stream.media with
        | `Audio params ->
            Internal_audio_decoder.create ~output ~faults ~log ~pcm_kind
              ~channels params
        | _ -> unsupported stream)
  | `Internal_video format -> (
      match stream.media with
        | `Video (params, _) ->
            let width, height = Display.expected_size params in
            ignore
              (Frame.video_dimensions
                 ~ideal_size:{ Frame.width; height; source = "ffmpeg decoder" }
                 ());
            let width, height = Content.Video.dimensions_of_format format in
            Internal_video_decoder.create ~output ~faults ~log ~source_id ~width
              ~height
              ~alpha:
                (try Content.Video.alpha_of_format format with _ -> false)
              stream params
        | _ -> unsupported stream)
  | `Text_subtitles ->
      Internal_subtitle_decoder.create ~output ~faults ~field:stream.field
  | `Bitmap_subtitles format -> (
      match stream.media with
        | `Subtitle params ->
            let width, height = Content.Video.dimensions_of_format format in
            Bitmap_subtitle_decoder.create ~output ~faults ~field:stream.field
              ~width ~height params
        | _ -> unsupported stream)
  | `Raw_audio format ->
      Raw_decoder.audio ~output ~faults ~log ~source_id ~format stream
  | `Raw_video format -> (
      match stream.media with
        | `Video (params, _) ->
            Raw_decoder.video ~output ~faults ~log ~source_id ~format stream
              params
        | _ -> unsupported stream)

let can_map ~ctype (description : Stream_description.t) =
  try
    List.iter (fun stream -> ignore (choose ~ctype stream)) description.streams;
    true
  with Failure _ -> false

let needs_key kind (stream : Stream_description.stream) =
  kind = `Copy
  &&
    match stream.media with
    | `Video _ -> (
        match Stream_description.copy_params stream with
          | Some params -> not (Copy_params.intra_only params)
          | None -> false)
    | _ -> false

let build_mapping t =
  let source_id = Source_id.make () in
  t.source_id <- source_id;
  t.entries <-
    List.filter_map
      (fun (stream : Stream_description.stream) ->
        match choose ~ctype:t.ctype stream with
          | None -> None
          | Some choice ->
              let kind = kind_of_choice choice in
              let per_stream =
                create_decoder ~output:(output t stream.field) ~faults:t.faults
                  ~source_id stream choice
              in
              Some
                {
                  stream;
                  kind;
                  per_stream;
                  needs_key = needs_key kind stream;
                  origin = 0;
                  last_raw_key = None;
                  crossed = t.events;
                  threshold = None;
                  held = [];
                  released = false;
                  progress = None;
                  latest_start = None;
                  previous_key = None;
                  skipped = 0;
                  queue = Queue.create ();
                })
      t.description.streams;
  t.started <- false;
  t.mapping_start <- None;
  t.event <- None;
  t.early_sparse <- [];
  t.reached <- 0;
  t.sparse_reached <- 0

let entry t index =
  List.find_opt (fun e -> e.stream.Stream_description.index = index) t.entries

let item_of_result : Av.input_result -> int * Per_stream.item = function
  | `Audio_packet (i, p) -> (i, `Packet (`Audio p))
  | `Video_packet (i, p) -> (i, `Packet (`Video p))
  | `Subtitle_packet (i, p) -> (i, `Packet (`Subtitle p))
  | `Data_packet (i, p) -> (i, `Data_packet p)
  | `Audio_frame (i, f) -> (i, `Audio_frame f)
  | `Video_frame (i, f) -> (i, `Video_frame f)
  | `Subtitle_frame (i, f) -> (i, `Subtitle_frame f)

let packet_times p =
  ( Avcodec.Packet.get_dts p,
    Avcodec.Packet.get_pts p,
    Avcodec.Packet.get_duration p,
    List.mem `Keyframe (Avcodec.Packet.get_flags p) )

let frame_times f =
  let key =
    match Avutil.Frame.pts f with
      | Some _ as pts -> pts
      | None -> Avutil.Frame.best_effort_timestamp f
  in
  (key, key, Avutil.Frame.duration f, true)

let queued t entry item =
  let tb = entry.stream.Stream_description.time_base in
  let tb, (raw_key, pts, duration, key_packet) =
    match item with
      | `Packet (`Audio p) -> (tb, packet_times p)
      | `Packet (`Video p) -> (tb, packet_times p)
      | `Packet (`Subtitle p) -> (tb, packet_times p)
      | `Data_packet p -> (tb, packet_times p)
      | `Audio_frame f -> (tb, frame_times f)
      | `Video_frame f -> (tb, frame_times f)
      | `Subtitle_frame f ->
          let pts = Avutil.Subtitle.get_pts f in
          (micro, (pts, pts, None, true))
  in
  t.next_seq <- t.next_seq + 1;
  let key = to_main ~src:tb raw_key in
  let pts_main =
    to_main ~src:tb (match pts with Some _ -> pts | None -> raw_key)
  in
  let item_end =
    match (pts, duration) with
      | Some pts, Some d -> to_main ~src:tb (Some (Int64.add pts d))
      | Some _, None -> pts_main
      | None, _ -> key
  in
  {
    seq = t.next_seq;
    item;
    raw_key = Option.map Int64.to_int raw_key;
    key;
    pts = pts_main;
    item_end;
    key_packet;
  }

let item_fault t message =
  match Faults.item t.faults ~kind:"item" message with
    | `Item_fault -> ()
    | `Source_fault ->
        raise
          (Source_fault
             (Printf.sprintf "%d consecutive invalid items"
                Faults.max_consecutive_faults))

let minimum_released t field =
  match
    List.filter_map
      (fun e -> if e.released then field e else None)
      (non_sparse t)
  with
    | [] -> None
    | l -> Some (List.fold_left min max_int l)

let advance_sparse t =
  Option.iter
    (fun p -> t.reached <- max t.reached p)
    (minimum_released t (fun e -> e.progress));
  match minimum_released t (fun e -> e.latest_start) with
    | Some s when s > t.sparse_reached ->
        t.sparse_reached <- s;
        List.iter
          (fun e ->
            if e.per_stream.Per_stream.sparse then e.per_stream.advance s)
          t.entries
    | _ -> ()

let push t entry q =
  let position =
    if entry.per_stream.Per_stream.sparse then
      Option.map (fun pts -> pts - entry.origin) q.pts
    else Option.map (fun key -> key - entry.origin) q.key
  in
  (match entry.per_stream.push ~position q.item with
    | `Ok -> ()
    | `Item_fault message -> item_fault t message
    | `Source_fault message -> raise (Source_fault message));
  if not entry.per_stream.Per_stream.sparse then (
    (match q.item_end with
      | Some e ->
          let p = e - entry.origin in
          entry.progress <-
            Some (match entry.progress with Some p' -> max p p' | None -> p)
      | None -> ());
    Option.iter
      (fun key ->
        let s = key - entry.origin in
        entry.latest_start <-
          Some (match entry.latest_start with Some s' -> max s s' | None -> s))
      q.key;
    advance_sparse t)

(* The mapping origin is fixed by the first item a non-sparse stream
   releases; sparse items released before it wait for it. *)
let deliver t entry q =
  if not entry.released then (
    entry.released <- true;
    if entry.skipped > 0 then
      log#debug "Stream %s: %d items before the threshold" (field_name entry)
        entry.skipped);
  let sparse = entry.per_stream.Per_stream.sparse in
  match t.mapping_start with
    | None when sparse -> t.early_sparse <- (entry, q) :: t.early_sparse
    | None -> (
        match q.key with
          | None -> push t entry q
          | Some key ->
              t.mapping_start <- Some key;
              List.iter (fun e -> e.origin <- key) t.entries;
              push t entry q;
              let early = List.rev t.early_sparse in
              t.early_sparse <- [];
              List.iter (fun (e, q) -> push t e q) early)
    | Some _ -> push t entry q

let release t entry q =
  match entry.threshold with
    | None -> deliver t entry q
    | Some threshold ->
        let pts = match q.pts with Some p -> Some p | None -> q.key in
        let clear () = entry.threshold <- None in
        if entry.needs_key then (
          match (q.key_packet, pts) with
            | true, Some p when p <= threshold ->
                entry.skipped <- entry.skipped + List.length entry.held;
                entry.held <- [q]
            | true, _ ->
                clear ();
                let held = List.rev entry.held in
                entry.held <- [];
                List.iter (deliver t entry) (held @ [q])
            | false, _ when entry.held = [] ->
                entry.skipped <- entry.skipped + 1
            | false, Some p when p >= threshold ->
                clear ();
                let held = List.rev (q :: entry.held) in
                entry.held <- [];
                List.iter (deliver t entry) held
            | false, _ -> entry.held <- q :: entry.held)
        else (
          match match q.item_end with Some e -> Some e | None -> pts with
            | Some e when e >= threshold ->
                clear ();
                deliver t entry q
            | _ -> entry.skipped <- entry.skipped + 1)

let release_key entry q =
  match q.key with Some k -> Some k | None -> entry.previous_key

let merged_queue t =
  let all =
    List.concat_map
      (fun e ->
        let l = List.of_seq (Queue.to_seq e.queue) in
        Queue.clear e.queue;
        let previous = ref None in
        List.map
          (fun q ->
            let k = match q.key with Some k -> Some k | None -> !previous in
            previous := k;
            (e, q, k))
          l)
      t.entries
  in
  List.stable_sort
    (fun (_, q, k) (_, q', k') ->
      match (k, k') with
        | Some k, Some k' when k <> k' -> compare k k'
        | _ -> compare q.seq q'.seq)
    all

let first_key e = Seq.find_map (fun q -> q.key) (Queue.to_seq e.queue)
let start_key_tolerance () = Frame.main_of_audio 1024

(* The key packet a copied video stream starts from (decoder.md §8.5, mixed
   case): its PTS and DTS, [None] while no queued key packet decides it. *)
let start_key ~t0 e =
  let keys =
    List.filter_map
      (fun q ->
        match (q.key_packet, q.pts, q.key) with
          | true, Some pts, Some dts -> Some (pts, dts)
          | _ -> None)
      (List.of_seq (Queue.to_seq e.queue))
  in
  let before =
    List.fold_left
      (fun found (pts, dts) -> if pts <= t0 then Some (pts, dts) else found)
      None keys
  in
  match before with
    | Some (pts, _) as found when t0 - pts <= start_key_tolerance () -> found
    | _ -> List.find_opt (fun (pts, _) -> pts >= t0) keys

let start_point t ~t0 =
  let copied_video =
    List.filter (fun e -> e.kind = `Copy && e.needs_key) (non_sparse t)
  in
  match (copied_video, t0) with
    | [], _ | _, None -> `Threshold t0
    | l, Some t0 ->
        let keys = List.map (start_key ~t0) l in
        if List.mem None keys then `Unknown
        else (
          match
            List.fold_left
              (fun latest k ->
                match (latest, k) with
                  | Some (pts, _), Some (pts', _) when pts' <= pts -> latest
                  | _, k -> k)
              None keys
          with
            | Some (pts, dts) -> `Start_point (pts, dts)
            | None -> `Threshold (Some t0))

let start t ~case =
  let decoded = List.filter (fun e -> e.kind = `Decoded) (non_sparse t) in
  let firsts = List.filter_map first_key decoded in
  let t0 =
    match (case, firsts) with
      | _, [] -> None
      | `All_seen, l -> Some (List.fold_left max min_int l)
      | (`End | `Discontinuity | `Window), l ->
          Some (List.fold_left min max_int l)
  in
  let t0 =
    match (t.target, t0) with
      | Some target, Some t0 -> Some (max target t0)
      | Some target, None -> Some target
      | None, t0 -> t0
  in
  let point =
    match (start_point t ~t0, case) with
      | `Unknown, `All_seen -> `Unknown
      | `Unknown, _ -> `Threshold t0
      | point, _ -> point
  in
  match point with
    | `Unknown -> ()
    | (`Threshold _ | `Start_point _) as point ->
        t.started <- true;
        (match point with
          | `Threshold t0 -> List.iter (fun e -> e.threshold <- t0) t.entries
          | `Start_point (pts, dts) ->
              List.iter
                (fun e ->
                  e.threshold <- Some (if e.needs_key then pts else dts))
                t.entries;
              t.mapping_start <- Some dts;
              List.iter (fun e -> e.origin <- dts) t.entries);
        if non_sparse t = [] then (
          let b = Option.value ~default:t.container_origin t.target in
          t.mapping_start <- Some b;
          List.iter (fun e -> e.origin <- b) t.entries);
        List.iter (fun (e, q, _) -> release t e q) (merged_queue t)

let queue_span t =
  let keys =
    List.concat_map
      (fun e ->
        List.filter_map (fun q -> q.key) (List.of_seq (Queue.to_seq e.queue)))
      t.entries
  in
  match keys with
    | [] -> 0
    | k :: l -> List.fold_left max k l - List.fold_left min k l

let max_span () =
  Frame.main_of_seconds Decoder_settings.max_interleave_duration#get

let drop_oldest t =
  match
    List.filter_map
      (fun e ->
        match Queue.peek_opt e.queue with Some q -> Some (e, q) | None -> None)
      t.entries
  with
    | [] -> ()
    | heads ->
        let e, _ =
          List.fold_left
            (fun (e, q) (e', q') ->
              match (q.key, q'.key) with
                | Some k, Some k' when k' < k -> (e', q')
                | None, Some _ -> (e', q')
                | _ -> (e, q))
            (List.hd heads) heads
        in
        ignore (Queue.pop e.queue);
        t.window_dropped <- t.window_dropped + 1

let enqueue t entry q =
  Queue.push q entry.queue;
  let seen e = e.per_stream.Per_stream.sparse || first_key e <> None in
  if List.for_all seen t.entries then start t ~case:`All_seen;
  if (not t.started) && List.for_all seen t.entries then (
    if queue_span t > max_span () then start t ~case:`Window)
  else if not t.started then
    while queue_span t > max_span () do
      if not t.window_warned then (
        t.window_warned <- true;
        log#important
          "Streams %s not seen within %.2fs: dropping the oldest queued items."
          (String.concat ", "
             (List.map field_name
                (List.filter (fun e -> not (seen e)) t.entries)))
          Decoder_settings.max_interleave_duration#get);
      drop_oldest t
    done

let params_unchanged t =
  let current =
    List.map
      (fun (index, _, p) -> (index, Copy_params.make (`Audio p)))
      (Av.get_audio_streams t.container)
    @ List.map
        (fun (index, _, p) -> (index, Copy_params.make (`Video p)))
        (Av.get_video_streams t.container)
  in
  List.for_all
    (fun (s : Stream_description.stream) ->
      let before =
        match s.media with
          | `Audio p -> Some (Copy_params.make (`Audio p))
          | `Video (p, _) -> Some (Copy_params.make (`Video p))
          | _ -> None
      in
      match (List.assoc_opt s.index current, before) with
        | Some now, Some before -> Copy_params.equal now before
        | _ -> true)
    t.description.streams

let cross t entry event =
  entry.origin <- entry.origin + event.shift;
  entry.crossed <- event.number;
  entry.last_raw_key <- None;
  entry.per_stream.switch ~source_id:t.source_id
    ~continuing:(t.ts_discont && params_unchanged t)

let discontinuity t entry q =
  if not t.started then start t ~case:`Discontinuity;
  let all_crossed number =
    List.for_all (fun e -> e.crossed >= number) (non_sparse t)
  in
  match t.event with
    | Some event when entry.crossed < event.number -> cross t entry event
    | Some event when not (all_crossed event.number) ->
        item_fault t
          (Printf.sprintf "Stream %s: decreasing timestamp after the jump"
             (field_name entry))
    | _ ->
        let key = Option.get q.key in
        let content_end = Option.value ~default:0 entry.progress in
        let event =
          { number = t.events + 1; shift = key - content_end - entry.origin }
        in
        t.events <- event.number;
        t.event <- Some event;
        t.had_discontinuity <- true;
        t.source_id <- Source_id.make ();
        log#info "Timestamp discontinuity on stream %s" (field_name entry);
        cross t entry event;
        List.iter
          (fun e -> if e.per_stream.Per_stream.sparse then cross t e event)
          t.entries

let is_ogg t =
  match t.description.format with
    | Some f -> String.length f >= 3 && String.sub f 0 3 = "ogg"
    | None -> false

(* Returns [false] when the item is dropped. *)
let check_monotonic t entry q =
  match (entry.last_raw_key, q.raw_key) with
    | Some last, Some k when k < last ->
        if t.ts_discont || is_ogg t then (
          discontinuity t entry q;
          entry.last_raw_key <- q.raw_key;
          true)
        else (
          item_fault t
            (Printf.sprintf "Stream %s: timestamp %d after %d dropped"
               (field_name entry) k last);
          false)
    | _, Some _ ->
        entry.last_raw_key <- q.raw_key;
        true
    | _, None -> true

let emit_metadata t ~previous list =
  match (list, previous) with
    | [], _ -> None
    | l, Some p when l = p -> None
    | l, Some _ -> Some (Some l)
    | l, None when t.decode_first_metadata -> Some (Some l)
    | l, None ->
        let request = Frame.Metadata.to_list t.request_metadata in
        if List.for_all (fun kv -> List.mem kv request) l then Some None
        else Some (Some l)

let add_metadata t m =
  match t.buffer with
    | Some b ->
        Generator.add_metadata b.Decoder.generator (Frame.Metadata.from_list m)
    | None -> ()

let container_metadata t =
  let list = Av.get_input_metadata t.container in
  match emit_metadata t ~previous:t.container_metadata list with
    | None -> ()
    | Some m ->
        t.container_metadata <- Some list;
        Option.iter (add_metadata t) m

let stream_metadata t entry =
  let index = entry.stream.Stream_description.index in
  let read stream =
    let list = Av.get_metadata stream in
    if list <> [] then Av.set_metadata stream [];
    list
  in
  let list =
    match entry.stream.media with
      | `Audio _ -> (
          match
            List.find_opt
              (fun (i, _, _) -> i = index)
              (Av.get_audio_streams t.container)
          with
            | Some (_, s, _) -> read s
            | None -> [])
      | `Video _ -> (
          match
            List.find_opt
              (fun (i, _, _) -> i = index)
              (Av.get_video_streams t.container)
          with
            | Some (_, s, _) -> read s
            | None -> [])
      | _ -> []
  in
  match
    emit_metadata t ~previous:(Hashtbl.find_opt t.stream_metadata index) list
  with
    | None -> ()
    | Some m ->
        Hashtbl.replace t.stream_metadata index list;
        Option.iter (add_metadata t) m

let streams_of_kind t kind get =
  List.filter_map
    (fun (index, stream, _) ->
      match entry t index with
        | Some e when e.kind = kind -> Some stream
        | _ -> None)
    (get t.container)

let read t =
  Av.read_input
    ~audio_packet:(streams_of_kind t `Copy Av.get_audio_streams)
    ~video_packet:(streams_of_kind t `Copy Av.get_video_streams)
    ~subtitle_packet:(streams_of_kind t `Copy Av.get_subtitle_streams)
    ~data_packet:(streams_of_kind t `Data Av.get_data_streams)
    ~audio_frame:(streams_of_kind t `Decoded Av.get_audio_streams)
    ~video_frame:(streams_of_kind t `Decoded Av.get_video_streams)
    ~subtitle_frame:(streams_of_kind t `Decoded Av.get_subtitle_streams)
    t.container

let rec step t =
  container_metadata t;
  match read t with
    | result -> (
        let index, item = item_of_result result in
        match entry t index with
          | None -> step t
          | Some entry ->
              stream_metadata t entry;
              let q = queued t entry item in
              if check_monotonic t entry q then (
                entry.previous_key <- release_key entry q;
                if t.started then release t entry q else enqueue t entry q))
    | exception Avutil.Error `Eagain -> step t
    | exception Avutil.Error `Invalid_data ->
        item_fault t "Invalid data read from the input";
        step t
    | exception Avutil.Error `Eof -> raise End_of_file
    | exception Avutil.Error `Exit -> raise Aborted
    | exception Avutil.Error e ->
        raise (Source_fault (Avutil.string_of_error e))

let locked t f = Mutex.protect t.mutex f

let decode t buffer =
  locked t (fun () ->
      t.buffer <- Some buffer;
      step t)

(* The track ends at the end of its longest field; shorter copy fields end
   with a gap, which carries no data. *)
let pad_fields t buffer =
  let generator = buffer.Decoder.generator in
  let fields = List.filter (fun e -> e.kind = `Copy) t.entries in
  let longest =
    List.fold_left
      (fun l e -> max l (Generator.field_length generator e.stream.field))
      0 fields
  in
  List.iter
    (fun e ->
      let field = e.stream.Stream_description.field in
      let missing = longest - Generator.field_length generator field in
      if missing > 0 then
        Generator.put generator field
          (Ffmpeg_content.Copy.lift_data
             (Ffmpeg_content.Copy.Data.make ~length:missing
                (Stream_description.copy_params e.stream))))
    fields

let eof t buffer =
  locked t (fun () ->
      t.buffer <- Some buffer;
      (try
         if not t.started then start t ~case:`End;
         List.iter (fun e -> e.per_stream.Per_stream.flush ()) t.entries
       with exn ->
         log#severe "Error while draining the decoder: %s"
           (Printexc.to_string exn));
      pad_fields t buffer;
      Generator.add_track_mark buffer.generator)

let position t =
  match t.mapping_start with
    | Some b when List.exists (fun e -> e.released) (non_sparse t) ->
        b - t.container_origin + t.reached
    | _ -> (
        match t.target with
          | Some target -> target - t.container_origin
          | None -> 0)

let seek t x =
  locked t (fun () ->
      if t.had_discontinuity then (
        log#important "Cannot seek after a timestamp discontinuity.";
        0)
      else (
        let u0 = position t in
        let u = max 0 (u0 + x) in
        let target = t.container_origin + u in
        log#important "Seeking to %.02f sec" (Frame.seconds_of_main u);
        let ts = max 0 (target - Frame.main_of_seconds seek_preroll) in
        let ts =
          Option.value ~default:0
            (Time_base.rescale ~src:(main ()) ~dst:micro ts)
        in
        match
          Av.seek ~fmt:`Microsecond ~ts:(Int64.of_int ts)
            ~max_ts:(Int64.of_int ts) t.container
        with
          | () ->
              build_mapping t;
              t.target <- Some target;
              u - u0
          | exception exn ->
              Faults.uncounted t.faults ~kind:"seek"
                (Printf.sprintf "Seek failed: %s" (Printexc.to_string exn));
              0))

let remaining t =
  match Av.get_input_duration ~format:`Millisecond t.container with
    | Some d ->
        let d = Frame.main_of_seconds (Int64.to_float d /. 1000.) in
        max 0 (d - position t)
    | None -> -1
    | exception _ -> -1

let close t = locked t (fun () -> Av.close t.container)

let create ?(request_metadata = Frame.Metadata.empty) ~decode_first_metadata
    ~ctype ~description container =
  let origin =
    match Av.get_input_start_time ~format:`Microsecond container with
      | Some s ->
          Option.value ~default:0
            (Time_base.rescale_int64 ~src:micro ~dst:(main ()) s)
      | None -> 0
  in
  let ts_discont =
    match Av.get_input_format container with
      | Some f -> Av.Format.input_ts_discont f
      | None -> false
  in
  let t =
    {
      container;
      description;
      ctype;
      decode_first_metadata;
      request_metadata;
      faults = Faults.create log;
      container_origin = origin;
      ts_discont;
      entries = [];
      source_id = Source_id.none;
      started = false;
      mapping_start = None;
      target = None;
      next_seq = 0;
      event = None;
      events = 0;
      had_discontinuity = false;
      early_sparse = [];
      reached = 0;
      sparse_reached = 0;
      window_warned = false;
      window_dropped = 0;
      buffer = None;
      container_metadata = None;
      stream_metadata = Hashtbl.create 4;
      mutex = Mutex.create ();
    }
  in
  build_mapping t;
  t

let decoder t =
  {
    Decoder.decode = decode t;
    eof = eof t;
    seek = seek t;
    close = (fun () -> close t);
  }
