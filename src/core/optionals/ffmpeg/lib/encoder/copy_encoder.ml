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

module F = Liquidsoap_ffmpeg_formats.Encoder_format

exception
  Incompatible of { field : Frame.field; header : string; source : string }

let max_rounding_shift = 1

type stream =
  [ `Audio of (Avutil.output, Avutil.audio, [ `Packet ]) Av.stream
  | `Video of (Avutil.output, Avutil.video, [ `Packet ]) Av.stream
  | `Subtitle of (Avutil.output, Avutil.subtitle, [ `Packet ]) Av.stream ]

(* A packet as the track reads it: its output position and its timestamps in
   main ticks. *)
type held = {
  packet : Ffmpeg_content.copy_packet;
  position : int;
  key : bool;
  dts : int option;
  pts : int option;
  duration : int option;
  extent : int;
}

type tenure = {
  source_id : Source_id.t;
  needs_key : bool;
  mutable ready : bool;
  mutable key_pts : int option;
  mutable key_position : int option;
  mutable dropped_before_key : int;
  mutable held : held list;
  mutable trim_below : int option;
  mutable offset : int option;
  mutable last_dts : int option;
  mutable written : int;
  faults : Faults.t;
}

type group = {
  id : Source_id.t;
  mutable started : bool;
  mutable offset : int option;
  mutable wait_start : int;
}

type t = {
  log : Log.t;
  container : Avutil.output Avutil.container;
  whole_output : bool;
  stores_header : bool;
  groups : (Source_id.t, group) Hashtbl.t;
  mutable tracks : track list;
  mutable skipped : int;
}

and track = {
  shared : t;
  field : Frame.field;
  keyframe : F.keyframe;
  mutable stream : stream option;
  mutable header : Copy_params.t option;
  mutable intra : bool;
  mutable reordering : bool;
  mutable tenure : tenure option;
  mutable closed : tenure list;
  mutable track_end : int option;
  mutable last_stored : int option;
  on_packet : key:bool -> time:int option -> unit;
}

let create ~log ~whole_output ~stores_header container =
  {
    log;
    container;
    whole_output;
    stores_header;
    groups = Hashtbl.create 4;
    tracks = [];
    skipped = 0;
  }

let track t ~field ~keyframe ~on_packet =
  let track =
    {
      shared = t;
      field;
      keyframe;
      stream = None;
      header = None;
      intra = false;
      reordering = false;
      tenure = None;
      closed = [];
      track_end = None;
      last_stored = None;
      on_packet;
    }
  in
  t.tracks <- t.tracks @ [track];
  track

let name track = Frame.Fields.string_of_field track.field
let main () = Time_base.main ()

let data track frame =
  Ffmpeg_content.Copy.get_data (Frame.get frame track.field)

let first_params track frame =
  List.find_map
    (fun (_, (r : Ffmpeg_content.Copy.Data.run)) -> r.source_params)
    (data track frame).runs

let mk_stream track frame =
  match first_params track frame with
    | None -> failwith "Copy stream created without parameters"
    | Some params ->
        let container = track.shared.container in
        let stream =
          match params.Copy_params.media with
            | `Audio p -> `Audio (Av.new_stream_copy ~params:p container)
            | `Video p ->
                let s = Av.new_stream_copy ~params:p container in
                Av.set_avg_frame_rate s params.frame_rate;
                `Video s
            | `Subtitle p -> `Subtitle (Av.new_stream_copy ~params:p container)
        in
        track.stream <- Some stream;
        track.header <- Some params;
        track.intra <- Copy_params.intra_only params;
        track.reordering <- Copy_params.has_reordering params

let new_tenure track ~source_id ~continues =
  let needs_key =
    track.keyframe = `Wait_for_keyframe && (not track.intra) && not continues
  in
  {
    source_id;
    needs_key;
    ready = not needs_key;
    key_pts = None;
    key_position = None;
    dropped_before_key = 0;
    held = [];
    trim_below = None;
    offset = None;
    last_dts = None;
    written = 0;
    faults = Faults.create track.shared.log;
  }

let group t ~id ~position =
  match Hashtbl.find_opt t.groups id with
    | Some g -> g
    | None ->
        let g = { id; started = false; offset = None; wait_start = position } in
        Hashtbl.replace t.groups id g;
        g

let stream_time_base track =
  match track.stream with
    | Some (`Audio s) -> Av.get_time_base s
    | Some (`Video s) -> Av.get_time_base s
    | Some (`Subtitle s) -> Av.get_time_base s
    | None -> main ()

let rescale ~dst v = Option.get (Time_base.rescale ~src:(main ()) ~dst v)

let write_packet track ~time_base ~dts ~pts ~duration packet =
  let set p =
    let p = Avcodec.Packet.dup p in
    Avcodec.Packet.set_dts p (Some (Int64.of_int dts));
    Avcodec.Packet.set_pts p (Some (Int64.of_int pts));
    Avcodec.Packet.set_duration p (Option.map Int64.of_int duration);
    track.on_packet
      ~key:(List.mem `Keyframe (Avcodec.Packet.get_flags p))
      ~time:(Time_base.rescale ~src:time_base ~dst:(main ()) pts);
    p
  in
  match (track.stream, packet) with
    | Some (`Audio s), `Audio p -> Av.write_packet s time_base (set p)
    | Some (`Video s), `Video p -> Av.write_packet s time_base (set p)
    | Some (`Subtitle s), `Subtitle p -> Av.write_packet s time_base (set p)
    | _ -> failwith "Packet of another media than its stream"

exception Dropped of string

(* Every item fault of a tenure counts towards the group restart of §12.3. *)
let rec item_fault track tenure message =
  match Faults.item tenure.faults ~kind:(name track) message with
    | `Item_fault -> ()
    | `Source_fault -> restart_group track.shared tenure.source_id

and restart_group t id =
  t.log#important "Copied source %d keeps failing on %s: restarting its group"
    id
    (String.concat ", "
       (List.filter_map
          (fun track ->
            match track.tenure with
              | Some tenure when tenure.source_id = id -> Some (name track)
              | _ -> None)
          t.tracks));
  List.iter
    (fun track ->
      match track.tenure with
        | Some tenure when tenure.source_id = id ->
            track.tenure <-
              Some (new_tenure track ~source_id:id ~continues:false)
        | _ -> ())
    t.tracks;
  match Hashtbl.find_opt t.groups id with
    | Some g ->
        g.started <- false;
        g.offset <- None
    | None -> ()

(* timestamps.md §6.1, §6.3, §6.4. *)
let write track tenure ~offset h =
  let t = track.shared in
  try
    let dts, pts =
      match (h.dts, h.pts) with
        | Some d, Some p -> (d, p)
        | Some d, None -> (d, d)
        | None, Some _ when track.reordering ->
            raise (Dropped "packet without DTS on a stream with reordering")
        | None, Some p -> (p, p)
        | None, None ->
            let v = h.position - t.skipped - offset in
            (v, v)
    in
    if pts < dts then
      raise (Dropped (Printf.sprintf "PTS %d below DTS %d" pts dts));
    (match tenure.last_dts with
      | Some last when dts < last ->
          raise (Dropped (Printf.sprintf "decreasing DTS %d after %d" dts last))
      | Some last when dts = last -> raise (Dropped "equal DTS dropped")
      | Some last
        when dts - last > Frame.main_of_seconds Settings.max_keyframe_wait#get
        ->
          t.log#important "Track %s, source %d: DTS jumps forward by %.2fs"
            (name track) tenure.source_id
            (Frame.seconds_of_main (dts - last))
      | _ -> ());
    let dts' = dts + offset and pts' = pts + offset in
    (match track.track_end with
      | Some e when tenure.written = 0 && dts' < e ->
          t.log#info "Track %s, source %d: packet dropped as join overlap"
            (name track) tenure.source_id;
          raise Exit
      | _ -> ());
    let stored_tb, stored_dts, stored_pts, stored_duration =
      if Av.output_started t.container then (
        let tb = stream_time_base track in
        let d = rescale ~dst:tb dts' in
        let d =
          match track.last_stored with
            | Some last when d <= last ->
                if last + 1 - d > max_rounding_shift then
                  raise
                    (Dropped
                       (Printf.sprintf
                          "DTS needs more than %d unit of time base %s"
                          max_rounding_shift (Time_base.to_string tb)));
                last + 1
            | _ -> d
        in
        ( tb,
          d,
          max d (rescale ~dst:tb pts'),
          Option.map (rescale ~dst:tb) h.duration ))
      else (main (), dts', pts', h.duration)
    in
    (try
       write_packet track ~time_base:stored_tb ~dts:stored_dts ~pts:stored_pts
         ~duration:stored_duration h.packet
     with Avutil.Error (`Invalid_data | `Other -22) as exn ->
       raise (Dropped (Printexc.to_string exn)));
    track.last_stored <-
      Some
        (if Av.output_started t.container && stored_tb = main () then
           rescale ~dst:(stream_time_base track) dts'
         else stored_dts);
    tenure.last_dts <- Some dts;
    tenure.written <- tenure.written + 1;
    Faults.delivered tenure.faults;
    let extent = Option.value ~default:h.extent h.duration in
    track.track_end <-
      Some
        (match track.track_end with
          | Some e -> max e (dts' + extent)
          | None -> dts' + extent)
  with
    | Exit -> ()
    | Dropped message ->
        item_fault track tenure
          (Printf.sprintf "Track %s, source %d: %s" (name track)
             tenure.source_id message)

let candidate t track (f : held) =
  let anchor = f.position - t.skipped in
  let start =
    match track.track_end with Some e -> max anchor e | None -> anchor
  in
  let dts =
    match (f.dts, f.pts) with
      | Some d, _ | None, Some d -> d
      | None, None -> anchor
  in
  start - dts

let members t g =
  List.filter_map
    (fun track ->
      match track.tenure with
        | Some tenure when tenure.source_id = g.id -> Some (track, tenure, true)
        | _ -> (
            match
              List.find_opt
                (fun tenure -> tenure.source_id = g.id && tenure.ready)
                track.closed
            with
              | Some tenure -> Some (track, tenure, false)
              | None -> None))
    t.tracks

(* The offset a member writes with: its group's, copied when the group's is
   fixed or when a late member joins. *)
let member_offset (g : group) (tenure : tenure) =
  match (tenure.offset, g.offset) with
    | Some o, _ -> Some o
    | None, Some o ->
        tenure.offset <- Some o;
        Some o
    | None, None -> None

(* encoder-copy.md §12.3: held packets merged across members by output DTS,
   each member's own order kept. *)
let write_held t g members =
  ignore t;
  let queues =
    List.filter_map
      (fun (track, tenure, _) ->
        match member_offset g tenure with
          | None -> None
          | Some offset ->
              let held = List.rev tenure.held in
              tenure.held <- [];
              Some (track, tenure, offset, ref held))
      members
  in
  let key offset (h : held) =
    offset
    +
      match (h.dts, h.pts) with
      | Some d, _ | None, Some d -> d
      | None, None -> h.position
  in
  let rec drain () =
    let earliest =
      List.fold_left
        (fun best ((_, _, offset, held) as q) ->
          match (!held, best) with
            | [], _ -> best
            | h :: _, None -> Some (key offset h, q)
            | h :: _, Some (k, _) when key offset h < k -> Some (key offset h, q)
            | _ -> best)
        None queues
    in
    match earliest with
      | None -> ()
      | Some (_, (track, tenure, offset, held)) ->
          let h = List.hd !held in
          held := List.tl !held;
          write track tenure ~offset h;
          drain ()
  in
  drain ()

let fix_offset t g members =
  let candidates =
    List.filter_map
      (fun (track, tenure, _) ->
        match List.rev tenure.held with
          | f :: _ -> Some (tenure, candidate t track f)
          | [] -> None)
      members
  in
  match candidates with
    | [] -> ()
    | (_, c) :: l ->
        let offset = List.fold_left (fun o (_, c) -> max o c) c l in
        g.offset <- Some offset;
        List.iter
          (fun ((tenure : tenure), _) -> tenure.offset <- Some offset)
          candidates

let pts_of (h : held) = match h.pts with Some p -> Some p | None -> h.dts

let trim t g members ~threshold =
  let trimmed = ref 0 in
  List.iter
    (fun (_, tenure, _) ->
      let held = List.rev tenure.held in
      let kept =
        if tenure.key_pts <> None then (
          let last_key =
            List.fold_left
              (fun last (i, h) ->
                match pts_of h with
                  | Some p when h.key && p <= threshold -> Some i
                  | _ -> last)
              None
              (List.mapi (fun i h -> (i, h)) held)
          in
          match last_key with
            | Some i -> List.filteri (fun j _ -> j >= i) held
            | None -> held)
        else (
          let kept =
            List.filter
              (fun h ->
                match pts_of h with Some p -> p >= threshold | None -> true)
              held
          in
          if kept = [] then tenure.trim_below <- Some threshold;
          kept)
      in
      trimmed := !trimmed + List.length held - List.length kept;
      tenure.held <- List.rev kept)
    members;
  if !trimmed > 0 then
    t.log#info "Source %d: %d held packets dropped before the group start" g.id
      !trimmed

let sparse track =
  match track.header with
    | Some { Copy_params.media = `Subtitle _ } -> true
    | _ -> false

(* The offset waits for every non-sparse member's first packet, or for
   [force] (wait limit, flush): encoder-copy.md §12.3, Writing. *)
let settle_offset t g members ~force =
  let ready = List.filter (fun (_, tenure, _) -> tenure.ready) members in
  let holds (_, tenure, _) = tenure.held <> [] in
  let fixable =
    force
    ||
      match List.filter (fun (track, _, _) -> not (sparse track)) ready with
      | [] -> List.exists holds ready
      | non_sparse -> List.for_all holds non_sparse
  in
  if g.offset = None && fixable then fix_offset t g ready;
  write_held t g ready

let start_group t g members ~force =
  g.started <- true;
  let ready = List.filter (fun (_, tenure, _) -> tenure.ready) members in
  let keyed =
    List.filter_map
      (fun (_, tenure, _) ->
        match (tenure.needs_key, tenure.key_pts, tenure.key_position) with
          | true, Some pts, Some position -> Some (pts, position)
          | _ -> None)
      ready
  in
  (match keyed with
    | [] -> ()
    | (pts, position) :: l ->
        let threshold, key_position =
          List.fold_left
            (fun (p, pos) (p', pos') -> if p' > p then (p', pos') else (p, pos))
            (pts, position) l
        in
        trim t g ready ~threshold;
        if t.whole_output && List.length members = List.length t.tracks then
          t.skipped <- t.skipped + max 0 (key_position - g.wait_start));
  settle_offset t g members ~force

let end_of_frame t ~position =
  Hashtbl.iter
    (fun _ g ->
      let members = members t g in
      if not g.started then (
        let open_tenure = List.exists (fun (_, _, o) -> o) members in
        let all_ready =
          List.for_all (fun (_, tenure, _) -> tenure.ready) members
        in
        let waited =
          position - g.wait_start
          >= Frame.main_of_seconds Settings.max_keyframe_wait#get
        in
        if members <> [] && all_ready then start_group t g members ~force:waited
        else if members <> [] && waited then (
          t.log#important "Source %d: starting without the keyframe of %s" g.id
            (String.concat ", "
               (List.filter_map
                  (fun (track, tenure, _) ->
                    if tenure.ready then None else Some (name track))
                  members));
          start_group t g members ~force:true)
        else if not open_tenure then (
          List.iter
            (fun (track, tenure, _) ->
              if tenure.held <> [] then
                t.log#important
                  "Track %s, source %d: %d held packets dropped, the source \
                   never started"
                  (name track) g.id (List.length tenure.held);
              tenure.held <- [])
            members;
          Hashtbl.remove t.groups g.id))
      else if g.offset = None then
        settle_offset t g members
          ~force:
            (position - g.wait_start
            >= Frame.main_of_seconds Settings.max_keyframe_wait#get);
      if members = [] then Hashtbl.remove t.groups g.id)
    (Hashtbl.copy t.groups);
  List.iter (fun track -> track.closed <- []) t.tracks

let flush t ~position =
  Hashtbl.iter
    (fun _ g ->
      let members = members t g in
      if not g.started then start_group t g members ~force:true
      else settle_offset t g members ~force:true)
    (Hashtbl.copy t.groups);
  ignore position

let incompatible track header (source : Copy_params.t) =
  Copy_params.incompatibility header source <> None
  || track.shared.stores_header
     && not (Copy_params.header_compatible header source)

let switch_position t frame =
  let track_switch track =
    match track.header with
      | None -> None
      | Some header ->
          let current =
            ref (Option.map (fun tenure -> tenure.source_id) track.tenure)
          in
          List.find_map
            (fun (start, (r : Ffmpeg_content.Copy.Data.run)) ->
              if r.source_id = Source_id.none || Some r.source_id = !current
              then None
              else (
                current := Some r.source_id;
                match r.source_params with
                  | Some params when incompatible track header params ->
                      Some start
                  | _ -> None))
            (data track frame).runs
  in
  List.fold_left
    (fun found track ->
      match (found, track_switch track) with
        | Some a, Some b -> Some (min a b)
        | None, b -> b
        | a, None -> a)
    None t.tracks

let check_params track (source : Copy_params.t) =
  match track.header with
    | None -> ()
    | Some header ->
        if incompatible track header source then
          raise
            (Incompatible
               {
                 field = track.field;
                 header = Copy_params.to_string header;
                 source = Copy_params.to_string source;
               });
        List.iter
          (fun (property, old_value, new_value) ->
            track.shared.log#important
              "Track %s: %s differs (%s against %s); the stream keeps the \
               first header"
              (name track) property old_value new_value)
          (Copy_params.descriptive_differences header source)

let accept track tenure g h =
  if tenure.needs_key && not tenure.ready then
    if h.key then (
      tenure.ready <- true;
      if tenure.dropped_before_key > 0 then (
        tenure.key_pts <- pts_of h;
        tenure.key_position <- Some h.position;
        track.shared.log#info
          "Track %s, source %d: %d packets dropped before the keyframe"
          (name track) tenure.source_id tenure.dropped_before_key))
    else tenure.dropped_before_key <- tenure.dropped_before_key + 1;
  let trimmed =
    match (tenure.trim_below, pts_of h) with
      | Some threshold, Some p when p < threshold ->
          track.shared.log#info
            "Track %s, source %d: packet before the group start dropped"
            (name track) tenure.source_id;
          true
      | Some _, _ ->
          tenure.trim_below <- None;
          false
      | None, _ -> false
  in
  if tenure.ready && not trimmed then (
    match (g.started, member_offset g tenure) with
      | true, Some offset -> write track tenure ~offset h
      | _ -> tenure.held <- h :: tenure.held)

let packet_times (packet : Ffmpeg_content.copy_packet) =
  match packet with
    | `Audio p ->
        Avcodec.Packet.
          ( get_dts p,
            get_pts p,
            get_duration p,
            List.mem `Keyframe (get_flags p) )
    | `Video p ->
        Avcodec.Packet.
          ( get_dts p,
            get_pts p,
            get_duration p,
            List.mem `Keyframe (get_flags p) )
    | `Subtitle p ->
        Avcodec.Packet.
          ( get_dts p,
            get_pts p,
            get_duration p,
            List.mem `Keyframe (get_flags p) )

let read_run track ~position ~start (r : Ffmpeg_content.Copy.Data.run) =
  let t = track.shared in
  if r.source_id = Source_id.none then
    List.iter
      (fun _ ->
        Faults.uncounted (Faults.create t.log) ~kind:(name track)
          (Printf.sprintf "Track %s: packet without a source dropped"
             (name track)))
      r.items
  else (
    (match track.tenure with
      | Some tenure when tenure.source_id = r.source_id -> ()
      | previous ->
          Option.iter
            (fun tenure -> track.closed <- tenure :: track.closed)
            previous;
          Option.iter (check_params track) r.source_params;
          track.tenure <-
            Some
              (new_tenure track ~source_id:r.source_id ~continues:r.continues));
    let tenure = Option.get track.tenure in
    let g = group t ~id:r.source_id ~position:(position + start) in
    let to_main v =
      Option.bind v (Time_base.rescale_int64 ~src:r.time_base ~dst:(main ()))
    in
    let rec go = function
      | [] -> ()
      | (pos, packet) :: rest ->
          let dts, pts, duration, key = packet_times packet in
          let key = key || track.intra in
          let next = match rest with (p, _) :: _ -> p | [] -> r.length in
          let h =
            {
              packet;
              position = position + start + pos;
              key;
              dts = to_main dts;
              pts = to_main pts;
              duration = to_main duration;
              extent = max 0 (next - pos);
            }
          in
          accept track tenure g h;
          go rest
    in
    go r.items)

let encode track ~position frame =
  List.iter
    (fun (start, r) -> read_run track ~position ~start r)
    (data track frame).runs

let codec_attr track =
  match track.stream with
    | Some (`Audio s) -> Av.codec_attr s
    | Some (`Video s) -> Av.codec_attr s
    | Some (`Subtitle s) -> Av.codec_attr s
    | None -> None

let bitrate track =
  match track.stream with
    | Some (`Audio s) -> Av.bitrate s
    | Some (`Video s) -> Av.bitrate s
    | Some (`Subtitle s) -> Av.bitrate s
    | None -> None

let video_size track =
  match track.header with
    | Some { Copy_params.media = `Video p } ->
        Some (Avcodec.Video.get_width p, Avcodec.Video.get_height p)
    | _ -> None
