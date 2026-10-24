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

module D = Description

let mark_key = "liquidsoap_track_mark"
let max_buffer () = Frame.main_of_seconds Settings.filter_max_buffer#get
let now () = Unix.gettimeofday ()

type ('m, 'fmt) media = {
  media : D.media;
  buffer_filter : [ `Unattached ] Avfilter.filter;
  sink_filter : [ `Unattached ] Avfilter.filter;
  frame_format : 'm Avutil.frame -> 'fmt;
  same : 'fmt -> 'fmt -> bool;
  describe : 'fmt -> string;
  buffer_args : 'fmt -> Avfilter.args list;
  placeholder : unit -> Avfilter.args list;
  input_entries : Avfilter.inputs -> 'm Avfilter.input Avfilter.entries;
  output_entries : Avfilter.outputs -> 'm Avfilter.output Avfilter.entries;
  frames : Content.data -> (int * Avutil.rational * 'm Avutil.frame) list;
  input_pads :
    [ `Attached ] Avfilter.filter ->
    ([ `Attached ], 'm, [ `Input ]) Avfilter.pad list;
  output_pads :
    [ `Attached ] Avfilter.filter ->
    ([ `Attached ], 'm, [ `Output ]) Avfilter.pad list;
}

let main_tb () = `Rational (Time_base.main ())

(* §8.3: one rule for every buffer source of the layer. *)
let audio_buffer_args ~time_base (format : Avutil.Audio.frame_format) =
  let layout =
    match Avutil.Channel_layout.get_mask format.channel_layout with
      | Some mask -> mask
      | None ->
          Option.value ~default:0L
            (Avutil.Channel_layout.get_mask
               (Avutil.Channel_layout.get_default
                  (Avutil.Channel_layout.get_nb_channels format.channel_layout)))
  in
  [
    `Pair ("sample_rate", `Int format.sample_rate);
    `Pair ("time_base", `Rational time_base);
    `Pair ("channel_layout", `String (Printf.sprintf "0x%Lx" layout));
    `Pair
      ( "sample_fmt",
        `String
          (Option.value ~default:"none"
             (Avutil.Sample_format.get_name format.sample_format)) );
  ]

let runs_frames runs =
  List.concat_map
    (fun (start, (time_base, items)) ->
      List.map
        (fun (position, frame) -> (start + position, time_base, frame))
        items)
    runs

let audio : ([ `Audio ], Avutil.Audio.frame_format) media =
  {
    media = `Audio;
    buffer_filter = Avfilter.abuffer;
    sink_filter = Avfilter.abuffersink;
    frame_format = Avutil.Audio.frame_format;
    same = Avutil.Audio.same_frame_format;
    describe = Pixel_formats.describe_audio;
    buffer_args =
      (fun format -> audio_buffer_args ~time_base:(Time_base.main ()) format);
    placeholder =
      (fun () ->
        [
          `Pair ("sample_rate", `Int 44100);
          `Pair ("time_base", `Rational { Avutil.num = 1; den = 44100 });
          `Pair ("channel_layout", `String "stereo");
          `Pair ("sample_fmt", `String "s16");
        ]);
    input_entries = (fun inputs -> inputs.audio);
    output_entries = (fun outputs -> outputs.audio);
    frames =
      (fun data ->
        runs_frames
          (List.map
             (fun (start, (run : Ffmpeg_content.Audio.Data.run)) ->
               (start, (run.time_base, run.items)))
             (Ffmpeg_content.Audio.get_data data).runs));
    input_pads = (fun filter -> filter.io.inputs.audio);
    output_pads = (fun filter -> filter.io.outputs.audio);
  }

let video : ([ `Video ], Avutil.Video.frame_format) media =
  {
    media = `Video;
    buffer_filter = Avfilter.buffer;
    sink_filter = Avfilter.buffersink;
    frame_format = Avutil.Video.frame_format;
    same = Pixel_formats.same_video;
    describe = Pixel_formats.describe_video;
    buffer_args =
      (fun format ->
        Avfilter.Utils.video_buffer_args ~time_base:(Time_base.main ()) format);
    placeholder =
      (fun () ->
        [
          `Pair ("video_size", `String "16x16");
          `Pair ("pix_fmt", `String "yuv420p");
          `Pair ("time_base", main_tb ());
        ]);
    input_entries = (fun inputs -> inputs.video);
    output_entries = (fun outputs -> outputs.video);
    frames =
      (fun data ->
        runs_frames
          (List.map
             (fun (start, (run : Ffmpeg_content.Video.Data.run)) ->
               (start, (run.time_base, run.items)))
             (Ffmpeg_content.Video.get_data data).runs));
    input_pads = (fun filter -> filter.io.inputs.video);
    output_pads = (fun filter -> filter.io.outputs.video);
  }

(* The graph's view of one graph input and of one sink; their media-specific
   state lives in the closures. *)
type input = {
  i_element : D.element;
  i_source : Source.source;
  i_operator : Source.source;
  buffer_args : unit -> Avfilter.args list;
  knows_format : unit -> bool;
  launched : Avfilter.inputs -> unit;
  send_eof : unit -> unit;
  reset : forget_format:bool -> unit;
  held_span : unit -> int;
  drop_held : unit -> unit;
  fill_pushed : unit -> int;
  start_fill : unit -> unit;
  wake : unit -> unit;
}

type read_mode = [ `Need | `Out | `Drain ]

type sink = {
  s_element : D.element;
  s_media : D.media;
  field : Frame.field;
  mutable reached : bool;
  s_launched : Avfilter.outputs -> unit;
  read : read_mode -> unit;
  at_end : unit -> bool;
  end_generation : unit -> unit;
  s_wake : Frame.content_type -> unit;
}

type generation = {
  number : int;
  source_id : Source_id.t;
  mutable produced : bool;
  after_fault : bool;
}

type t = {
  description : D.t;
  id : string;
  log : Log.t;
  mutex : Mutex.t;
  faults : Faults.t;
  mutable inputs : input list;
  mutable sinks : sink list;
  mutable generation : generation option;
  mutable generations : int;
  mutable exhausted : bool;
  mutable failed_until : float option;
  mutable after_fault : bool;
  mutable buffer : Generator.t;
  mutable source : Source.source option;
  mutable pending_fault : (string * bool) option;
  mutable output_clock : Clock.t option;
  mutable input_clock : Clock.t option;
}

let counter = Atomic.make 0

let create () =
  let gid = Atomic.fetch_and_add counter 1 in
  let id = Printf.sprintf "ffmpeg.filter.%d" gid in
  let log = Log.make ["ffmpeg"; "filter"] in
  {
    description = D.create ~gid ();
    id;
    log;
    mutex = Mutex.create ();
    faults = Faults.create log;
    inputs = [];
    sinks = [];
    generation = None;
    generations = 0;
    exhausted = false;
    failed_until = None;
    after_fault = false;
    buffer = Generator.create Frame.Fields.empty;
    source = None;
    pending_fault = None;
    output_clock = None;
    input_clock = None;
  }

let locked t fn = Mutex.protect t.mutex fn
let inputs t = List.rev t.inputs
let sinks t = List.rev t.sinks
let running t = t.generation <> None

(* A source fault of the graph source; [drain] when the generation can still
   drain. *)
exception Graph_fault of { message : string; drain : bool }

(* §11.5. *)
let reset_after t ~forget_format =
  List.iter (fun i -> i.reset ~forget_format) t.inputs

(* §11.7: a generation that can still drain is drained into the buffer. *)
let end_generation t ~drain ~forget_format =
  match t.generation with
    | None -> reset_after t ~forget_format
    | Some _ ->
        if drain then (
          List.iter (fun i -> i.send_eof ()) (inputs t);
          List.iter (fun s -> s.read `Drain) (sinks t));
        List.iter (fun s -> s.end_generation ()) (sinks t);
        t.generation <- None;
        reset_after t ~forget_format

(* §11.6, source faults of the graph source. *)
let source_fault t ~drain message =
  t.log#severe "Filter graph %s failed: %s" t.id message;
  end_generation t ~drain ~forget_format:true;
  List.iter (fun i -> i.drop_held ()) t.inputs;
  Generator.add_track_mark t.buffer;
  t.after_fault <- false;
  t.failed_until <- Some (now () +. Faults.fault_retry_delay)

(* §11.6, an FFmpeg error in a running generation: an item fault, unless the
   generation was itself a rebuild after one and produced nothing. *)
let generation_error t message =
  match t.generation with
    | Some { after_fault = true; produced = false; _ } ->
        raise (Graph_fault { message; drain = false })
    | _ -> (
        match Faults.item t.faults ~kind:"filter" message with
          | `Source_fault -> raise (Graph_fault { message; drain = false })
          | `Item_fault ->
              List.iter (fun s -> s.end_generation ()) (sinks t);
              t.generation <- None;
              t.after_fault <- true;
              reset_after t ~forget_format:false)

let all_formats_known t = List.for_all (fun i -> i.knows_format ()) t.inputs

(* §11.3. *)
let build t =
  let config = Avfilter.init () in
  let launched =
    locked t (fun () ->
        D.replay t.description config;
        Avfilter.launch config)
  in
  t.generations <- t.generations + 1;
  t.log#info "Initializing graph %s (generation %d)" t.id t.generations;
  t.generation <-
    Some
      {
        number = t.generations;
        source_id = Source_id.make ();
        produced = false;
        after_fault = t.after_fault;
      };
  List.iter (fun s -> s.s_launched launched.outputs) (sinks t);
  List.iter (fun i -> i.launched launched.inputs) (inputs t)

let try_build t =
  if t.generation = None && all_formats_known t then (
    try build t with
      | Graph_fault _ as exn -> raise exn
      | exn ->
          t.generation <- None;
          raise
            (Graph_fault
               {
                 message = "Build failed: " ^ Printexc.to_string exn;
                 drain = false;
               }))

let failed t =
  match t.failed_until with
    | Some time when now () >= time ->
        t.failed_until <- None;
        false
    | Some _ -> true
    | None -> false

(* §12.1. *)
let is_ready t =
  Generator.length t.buffer > 0
  ||
  let inputs_ready = List.for_all (fun i -> i.i_source#is_ready) t.inputs in
  if t.exhausted && t.inputs <> [] && not inputs_ready then t.exhausted <- false;
  (not (failed t)) && (not t.exhausted) && inputs_ready

let input_clock t = Option.get t.input_clock
let field_short t size s = Generator.field_length t.buffer s.field < size

(* §12.3, output buffer. *)
let check_output_buffer t =
  let limit = max_buffer () in
  let dropped =
    List.fold_left
      (fun dropped s ->
        let length = Generator.field_length t.buffer s.field in
        if length > limit then (
          let excess = length - limit in
          let data = Generator.get_field t.buffer s.field in
          Generator.set_field t.buffer s.field (Content.sub data excess limit);
          dropped + excess)
        else dropped)
      0 (sinks t)
  in
  if dropped > 0 then
    Faults.uncounted t.faults ~kind:"output buffer"
      (Printf.sprintf
         "Filter graph %s has buffered more than %.2fs of data for one of its \
          outputs and dropped %.2fs. This happens when the graph's outputs are \
          consumed at diverging rates. Raise settings.ffmpeg.filter_max_buffer \
          if the graph legitimately produces that much on a single pass."
         t.id
         (Frame.seconds_of_main limit)
         (Frame.seconds_of_main dropped))

(* §12.3, held and fill input. *)
let check_inputs t =
  let limit = max_buffer () in
  List.iter
    (fun i ->
      if i.held_span () > limit then
        raise
          (Graph_fault
             {
               message =
                 Printf.sprintf
                   "%s holds more than %.2fs waiting for the other inputs"
                   i.i_element.name
                   (Frame.seconds_of_main limit);
               drain = true;
             });
      if i.fill_pushed () > limit then
        raise
          (Graph_fault
             {
               message =
                 Printf.sprintf
                   "%s pushed more than %.2fs in one fill without output"
                   i.i_element.name
                   (Frame.seconds_of_main limit);
               drain = true;
             }))
    t.inputs

(* §12.2. *)
let fill t =
  let size = Lazy.Mutexed.force Frame.size in
  List.iter (fun i -> i.start_fill ()) t.inputs;
  let rec loop () =
    if running t then List.iter (fun s -> s.read `Need) (sinks t);
    if List.exists (field_short t size) t.sinks then
      if running t && List.for_all (fun s -> s.at_end ()) t.sinks then (
        end_generation t ~drain:true ~forget_format:true;
        t.exhausted <- true)
      else if t.inputs = [] then (
        if not t.exhausted then (
          let before = Generator.length t.buffer in
          if not (running t) then try_build t;
          List.iter (fun s -> s.read `Need) (sinks t);
          if
            Generator.length t.buffer > before
            || List.exists (fun s -> s.at_end ()) t.sinks
          then loop ()))
      else if not (List.for_all (fun i -> i.i_source#is_ready) t.inputs) then (
        if running t then end_generation t ~drain:true ~forget_format:true)
      else (
        try_build t;
        Clock.tick (input_clock t);
        (match t.pending_fault with
          | Some (message, drain) ->
              t.pending_fault <- None;
              raise (Graph_fault { message; drain })
          | None -> ());
        check_inputs t;
        if running t then
          List.iter
            (fun s -> s.read (if s.reached then `Out else `Need))
            (sinks t);
        check_output_buffer t;
        loop ())
  in
  try loop ()
  with Graph_fault { message; drain } -> source_fault t ~drain message

(* Later entries win on a shared key. *)
let merge entries =
  List.rev
    (List.fold_left
       (fun acc (k, v) -> (k, v) :: List.remove_assoc k acc)
       [] entries)

type 'm held = {
  frame : 'm Avutil.frame;
  pts : int;
  duration : int option;
  metadata : (string * string) list;
  position : int;
}

let record_fault t message drain =
  if t.pending_fault = None then t.pending_fault <- Some (message, drain)

(* §13.1, one graph input. [take] runs on each input-clock tick with the
   frame of the input's source. *)
let make_input (type m fmt) t (media : (m, fmt) media) ~element ~pass_metadata
    ~field (source : Source.source) =
  let format : fmt option ref = ref None in
  let push = ref None in
  let eos = ref false in
  let held = Queue.create () in
  let timeline = ref 0 in
  let marks = ref 0 in
  let pending = ref [] in
  let pushed = ref 0 in
  let faults = Faults.create t.log in
  let send entry push_fn =
    let frame = entry.frame in
    let pts = Avutil.Frame.pts frame in
    let duration = Avutil.Frame.duration frame in
    let metadata = Avutil.Frame.metadata frame in
    Avutil.Frame.set_pts frame (Some (Int64.of_int entry.pts));
    Avutil.Frame.set_duration frame (Option.map Int64.of_int entry.duration);
    Avutil.Frame.set_metadata frame (merge (metadata @ entry.metadata));
    Fun.protect
      ~finally:(fun () ->
        Avutil.Frame.set_pts frame pts;
        Avutil.Frame.set_duration frame duration;
        Avutil.Frame.set_metadata frame metadata)
      (fun () -> locked t (fun () -> push_fn (`Frame frame)))
  in
  let rec deliver entry fmt =
    match (t.generation, !push) with
      | Some _, Some push_fn -> (
          match !format with
            | Some old when not (media.same old fmt) ->
                t.log#important
                  "The format of %s changed in the middle of a stream, from %s \
                   to %s: rebuilding the filter graph. Its filters start over."
                  element.D.name (media.describe old) (media.describe fmt);
                end_generation t ~drain:true ~forget_format:false;
                format := Some fmt;
                try_build t;
                deliver entry fmt
            | _ -> (
                format := Some fmt;
                try send entry push_fn
                with Avutil.Error e ->
                  generation_error t
                    (Printf.sprintf "Frame dropped at %s: %s" element.D.name
                       (Avutil.string_of_error e))))
      | _ ->
          if !format = None then format := Some fmt;
          Queue.push (entry, fmt) held;
          try_build t
  in
  let entries_of frame =
    let metadata =
      if pass_metadata then
        List.map
          (fun (p, m) -> (p, Frame.Metadata.to_list m))
          (Frame.get_all_metadata frame)
      else []
    in
    let marks =
      List.map
        (fun p ->
          incr marks;
          (p, [(mark_key, string_of_int !marks)]))
        (Frame.track_marks frame)
    in
    List.stable_sort (fun (a, _) (b, _) -> compare a b) (metadata @ marks)
  in
  let take frame =
    let base = !timeline in
    let entries = ref (entries_of frame) in
    List.iter
      (fun (position, time_base, f) ->
        let now, later =
          List.partition (fun (p, _) -> p <= position) !entries
        in
        entries := later;
        let metadata = merge (!pending @ List.concat_map snd now) in
        pending := [];
        match media.frame_format f with
          | exception exn -> (
              match
                Faults.item faults ~kind:"invalid frame"
                  (Printf.sprintf "Frame dropped at %s: %s" element.D.name
                     (Printexc.to_string exn))
              with
                | `Source_fault -> record_fault t "invalid frames" true
                | `Item_fault -> ())
          | fmt ->
              Faults.delivered faults;
              deliver
                {
                  frame = f;
                  pts = base + position;
                  duration =
                    Option.bind (Avutil.Frame.duration f)
                      (Time_base.rescale_int64 ~src:time_base
                         ~dst:(Time_base.main ()));
                  metadata;
                  position = base + position;
                }
                fmt)
      (media.frames (Frame.get frame field));
    pending := !pending @ List.concat_map snd !entries;
    timeline := base + Frame.position frame;
    pushed := !pushed + Frame.position frame
  in
  let take frame =
    try take frame
    with Graph_fault { message; drain } -> record_fault t message drain
  in
  let launched (inputs : Avfilter.inputs) =
    push := Some (List.assoc element.D.name (media.input_entries inputs));
    eos := false;
    let frames = List.of_seq (Queue.to_seq held) in
    Queue.clear held;
    List.iter (fun (entry, fmt) -> deliver entry fmt) frames
  in
  let input =
    {
      i_element = element;
      i_source = source;
      i_operator = source;
      buffer_args =
        (fun () ->
          match !format with
            | Some fmt -> media.buffer_args fmt
            | None -> media.placeholder ());
      knows_format = (fun () -> !format <> None);
      launched;
      send_eof =
        (fun () ->
          match !push with
            | Some push_fn when not !eos -> (
                eos := true;
                try locked t (fun () -> push_fn `Flush)
                with Avutil.Error e ->
                  t.log#important "End of stream refused at %s: %s"
                    element.D.name (Avutil.string_of_error e))
            | _ -> ());
      reset =
        (fun ~forget_format ->
          push := None;
          eos := false;
          if forget_format then format := None);
      held_span =
        (fun () ->
          match Queue.peek_opt held with
            | Some (entry, _) -> !timeline - entry.position
            | None -> 0);
      drop_held = (fun () -> Queue.clear held);
      fill_pushed = (fun () -> !pushed);
      start_fill = (fun () -> pushed := 0);
      wake =
        (fun () ->
          timeline := 0;
          marks := 0;
          pending := [];
          format := None;
          push := None;
          eos := false;
          Queue.clear held);
    }
  in
  (input, take)

(* §14.2: what one sink frame carries out of the graph. *)
type entry = [ `Mark | `Metadata of (string * string) list ]

let place t field entries =
  let pos = Generator.field_length t.buffer field in
  List.iter
    (function
      | `Mark -> Generator.add_track_mark ~pos t.buffer
      | `Metadata m ->
          Generator.add_metadata ~pos t.buffer (Frame.Metadata.from_list m))
    entries

let sink_entries ~seen ~pass_metadata ~time_base frame : entry list =
  let metadata = Avutil.Frame.metadata frame in
  let mark =
    match List.assoc_opt mark_key metadata with
      | Some n when not (Hashtbl.mem seen n) ->
          Hashtbl.replace seen n ();
          [`Mark]
      | _ -> []
  in
  let own = List.filter (fun (k, _) -> k <> mark_key) metadata in
  if pass_metadata && own <> [] then (
    let seconds key = function
      | Some v ->
          [
            ( key,
              Utils.string_of_float
                (Int64.to_float v *. float time_base.Avutil.num
               /. float time_base.Avutil.den) );
          ]
      | None -> []
    in
    let timestamps =
      seconds "lavfi.liq.pts" (Avutil.Frame.pts frame)
      @ seconds "lavfi.liq.duration" (Avutil.Frame.duration frame)
      @ seconds "lavfi.liq.best_effort_timestamp"
          (Avutil.Frame.best_effort_timestamp frame)
    in
    mark @ [`Metadata (own @ timestamps)])
  else mark

let generation_source_id t =
  match t.generation with Some g -> g.source_id | None -> Source_id.none

let mark_produced t =
  match t.generation with Some g -> g.produced <- true | None -> ()

(* The shared reading loop of §12.2 and §11.7 over one sink handler. *)
let read_sink t ~field ~handler ~at_end ~process mode =
  match !handler with
    | None -> ()
    | Some (h : unit -> _) ->
        let size = Lazy.Mutexed.force Frame.size in
        let start = Generator.field_length t.buffer field in
        let limit = max_buffer () in
        let rec go () =
          let read = Generator.field_length t.buffer field - start in
          let stop =
            match mode with
              | `Need -> Generator.field_length t.buffer field >= size
              | `Out -> false
              | `Drain ->
                  read > limit
                  &&
                  (t.log#important
                     "Filter graph %s: drain read of %s stopped after %.2fs."
                     t.id
                     (Frame.Fields.string_of_field field)
                     (Frame.seconds_of_main read);
                   true)
          in
          if (not stop) && not !at_end then (
            match locked t h with
              | frame ->
                  process frame;
                  go ()
              | exception Avutil.Error `Eagain -> ()
              | exception Avutil.Error `Eof -> at_end := true
              | exception Avutil.Error e -> (
                  let message =
                    Printf.sprintf "Reading %s failed: %s"
                      (Frame.Fields.string_of_field field)
                      (Avutil.string_of_error e)
                  in
                  match mode with
                    | `Drain -> t.log#important "%s" message
                    | `Need | `Out -> generation_error t message))
        in
        go ()

let audio_sink t ~element ~field ~pass_metadata =
  let handler = ref None in
  let time_base = ref (Time_base.main ()) in
  let at_end = ref false in
  let conformer = ref None in
  let format = ref None in
  let start = ref 0 in
  let samples = ref 0 in
  let timeline = ref 0 in
  let seen = Hashtbl.create 10 in
  let pending = ref [] in
  let emit _ frame =
    let rate = (Avutil.Audio.frame_format frame).sample_rate in
    let at n =
      !start
      + Option.get
          (Time_base.rescale
             ~src:{ Avutil.num = 1; den = rate }
             ~dst:(Time_base.main ()) n)
    in
    let position = at !samples in
    samples := !samples + Avutil.Audio.frame_nb_samples frame;
    let next = at !samples in
    Avutil.Frame.set_pts frame (Some (Int64.of_int position));
    place t field !pending;
    pending := [];
    let format = Option.get !format in
    Generator.put t.buffer field
      (Ffmpeg_content.Audio.lift_data
         (Ffmpeg_content.Audio.Data.of_run
            (Ffmpeg_content.Audio.get_params format)
            (Ffmpeg_content.Audio.Data.run ~source_id:(generation_source_id t)
               ~time_base:(Time_base.main ()) ~length:(next - position)
               [(0, frame)])));
    timeline := next;
    mark_produced t
  in
  let process frame =
    pending :=
      !pending @ sink_entries ~seen ~pass_metadata ~time_base:!time_base frame;
    Conformer.Audio.convert (Option.get !conformer) ~time_base:!time_base emit
      frame
  in
  {
    s_element = element;
    s_media = `Audio;
    field;
    reached = false;
    s_launched =
      (fun outputs ->
        let output = List.assoc element.D.name outputs.Avfilter.audio in
        handler := Some output.handler;
        time_base := Avfilter.time_base output.context;
        at_end := false;
        Hashtbl.reset seen;
        start := !timeline;
        samples := 0);
    read = read_sink t ~field ~handler ~at_end ~process;
    at_end = (fun () -> !at_end);
    end_generation =
      (fun () ->
        Option.iter
          (fun c ->
            try Conformer.Audio.flush c emit
            with exn ->
              t.log#important "Draining %s failed: %s"
                (Frame.Fields.string_of_field field)
                (Printexc.to_string exn))
          !conformer;
        handler := None;
        at_end := false);
    s_wake =
      (fun content_type ->
        let f = Frame.Fields.find field content_type in
        format := Some f;
        conformer := Some (Conformer.Audio.create ~log:t.log f);
        handler := None;
        timeline := 0;
        pending := []);
  }

let video_sink t ~element ~field ~pass_metadata =
  let handler = ref None in
  let time_base = ref (Time_base.main ()) in
  let at_end = ref false in
  let conformer = ref None in
  let format = ref None in
  let timeline = ref 0 in
  let helper = ref None in
  let last_pts = ref None in
  let seen = Hashtbl.create 10 in
  let carried = ref [] in
  let entries = Queue.create () in
  let faults = Faults.create t.log in
  let accessors =
    {
      Duration.get_ts = Avutil.Frame.pts;
      set_ts = Avutil.Frame.set_pts;
      get_duration = Avutil.Frame.duration;
    }
  in
  let new_helper start =
    helper :=
      Some
        (Duration.create ~start ~convert_ts:false ~src:!time_base ~faults
           accessors)
  in
  let emit { Duration.length; items } =
    if length > 0 then (
      let position = !timeline in
      List.iter
        (fun f -> Avutil.Frame.set_pts f (Some (Int64.of_int position)))
        items;
      List.iter
        (fun _ ->
          match Queue.take_opt entries with
            | Some e -> place t field e
            | None -> ())
        items;
      let format = Option.get !format in
      Generator.put t.buffer field
        (Ffmpeg_content.Video.lift_data
           (Ffmpeg_content.Video.Data.of_run
              (Ffmpeg_content.Video.get_params format)
              (Ffmpeg_content.Video.Data.run ~source_id:(generation_source_id t)
                 ~time_base:(Time_base.main ()) ~length
                 (List.map (fun f -> (0, f)) items))));
      timeline := position + length;
      mark_produced t)
  in
  let flush_helper () =
    match !helper with
      | Some h ->
          helper := None;
          Option.iter emit (Duration.flush h);
          Some (Duration.end_position h)
      | None -> None
  in
  (* §13.3. *)
  let deliver frame =
    let pts = Avutil.Frame.pts frame in
    (match (!last_pts, pts) with
      | Some previous, Some current when current < previous ->
          Faults.uncounted faults ~kind:"decreasing pts"
            (Printf.sprintf
               "Filter graph %s output %s: decreasing timestamp %Ld after %Ld, \
                timeline repaired."
               t.id
               (Frame.Fields.string_of_field field)
               current previous);
          new_helper (Option.value ~default:!timeline (flush_helper ()))
      | _ -> ());
    if pts <> None then last_pts := pts;
    if !helper = None then new_helper !timeline;
    match Duration.push (Option.get !helper) frame with
      | `Chunk chunk -> emit chunk
      | `Nothing | `Dropped | `Discontinuity _ -> ()
  in
  let process frame =
    let own =
      !carried @ sink_entries ~seen ~pass_metadata ~time_base:!time_base frame
    in
    carried := own;
    Conformer.Video.convert (Option.get !conformer)
      (fun f ->
        Queue.push !carried entries;
        carried := [];
        deliver f)
      frame
  in
  {
    s_element = element;
    s_media = `Video;
    field;
    reached = false;
    s_launched =
      (fun outputs ->
        let output = List.assoc element.D.name outputs.Avfilter.video in
        handler := Some output.handler;
        time_base := Avfilter.time_base output.context;
        at_end := false;
        last_pts := None;
        Hashtbl.reset seen);
    read = read_sink t ~field ~handler ~at_end ~process;
    at_end = (fun () -> !at_end);
    end_generation =
      (fun () ->
        ignore (flush_helper ());
        handler := None;
        at_end := false);
    s_wake =
      (fun content_type ->
        let f = Frame.Fields.find field content_type in
        format := Some f;
        conformer := Some (Conformer.Video.create ~log:t.log f);
        handler := None;
        helper := None;
        timeline := 0;
        carried := [];
        Queue.clear entries);
  }

(* §8.2: an output, in the liquidsoap sense, of the input clock. *)
class graph_input ~name ~take (source : Source.source) =
  object (self)
    inherit Source.active_operator ~name [source]
    initializer Typing.(source#frame_type <: self#frame_type)
    method! source_type = `Output (self :> Source.active)
    method fallible = true
    method private can_generate_frame = source#is_ready
    method private generate_frame = source#get_frame
    method abort_track = source#abort_track
    method remaining = source#remaining
    method private self_sync = source#cached_self_sync
    method effective_source = source#effective_source
    method reset = ()
    method output = if self#is_ready then take self#get_frame
  end

(* §9.3. *)
class graph_source t =
  object (self)
    inherit Source.source ~name:"ffmpeg.filter" ()
    method fallible = true
    method private can_generate_frame = is_ready t

    method private generate_frame =
      let size = Lazy.Mutexed.force Frame.size in
      if List.exists (field_short t size) t.sinks then fill t;
      Generator.slice t.buffer size

    method remaining = Generator.remaining t.buffer
    method! seek _ = 0

    method abort_track =
      Generator.clear t.buffer;
      Generator.add_track_mark t.buffer

    method effective_source = (self :> Source.source)

    method private self_sync =
      Source_sync.of_sources (List.map (fun i -> i.i_source) t.inputs) ()

    initializer
      self#on_wake_up (fun () ->
          t.buffer <- Generator.create self#content_type;
          List.iter (fun s -> s.s_wake self#content_type) t.sinks;
          List.iter (fun i -> i.wake ()) t.inputs;
          t.exhausted <- false;
          t.failed_until <- None;
          t.after_fault <- false;
          t.pending_fault <- None;
          Clock.register ~parent:self#clock (input_clock t));
      self#on_sleep (fun () ->
          end_generation t ~drain:false ~forget_format:true;
          Clock.deregister ~parent:self#clock (input_clock t))
  end

let description t = t.description
let in_scope t = D.in_scope t.description

let add_instance t (filter : [ `Unattached ] Avfilter.filter) args =
  let e = D.element t.description filter.name in
  D.add t.description (fun _ config ->
      e.attached <- Some (Avfilter.attach ~args ~name:e.name filter config));
  D.expect_pads t.description e (D.attached e);
  e

let link_input t (src : 'm D.pad) (e : D.element) media index
    (input_pads : [ `Attached ] Avfilter.filter -> _ list) =
  D.consumed t.description `Input e media index;
  D.link t.description src ~dst:e
    ~dst_name:(Printf.sprintf "%s:%d" e.name index) (fun () ->
      List.nth (input_pads (D.attached e)) index)

let output_pad t (e : D.element) (media : ('m, _) media) : 'm D.pad =
  D.pad t.description e media.media 0 (fun () ->
      List.hd (media.output_pads (D.attached e)))

(* §8.2. *)
let add_input t media ~id ~pass_metadata
    ((field, source) : Frame.field * Source.source) =
  let e = D.element t.description "input" in
  let input, take =
    make_input t media ~element:e ~pass_metadata ~field source
  in
  let operator = new graph_input ~name:id ~take source in
  t.inputs <-
    { input with i_operator = (operator :> Source.source) } :: t.inputs;
  D.add t.description (fun mode config ->
      let args =
        match mode with
          | `Probe -> media.placeholder ()
          | `Build -> input.buffer_args ()
      in
      e.attached <-
        Some (Avfilter.attach ~args ~name:e.name media.buffer_filter config));
  D.expect t.description e `Output media.media 0 "default";
  output_pad t e media

let graph_source t =
  match t.source with
    | Some s -> s
    | None ->
        let s = (new graph_source t :> Source.source) in
        t.source <- Some s;
        s

(* §9.2. *)
let add_output t media ~make_sink ~pass_metadata (pad : 'm D.pad) =
  let source = graph_source t in
  let count =
    List.length (List.filter (fun s -> s.s_media = media.media) t.sinks)
  in
  let field =
    match media.media with
      | `Audio -> Frame.Fields.audio_n count
      | `Video -> Frame.Fields.video_n count
  in
  let e = D.element t.description "output" in
  D.add t.description (fun _ config ->
      e.attached <- Some (Avfilter.attach ~name:e.name media.sink_filter config));
  D.link t.description pad ~dst:e ~dst_name:e.name (fun () ->
      List.hd (media.input_pads (D.attached e)));
  t.sinks <- make_sink t ~element:e ~field ~pass_metadata :: t.sinks;
  (field, source)

(* The filter names of a filtergraph description: each filter of each chain,
   its link labels and instance name stripped. *)
let description_filters description =
  let strip_labels s =
    let s = String.trim s in
    let rec drop s =
      if String.length s > 0 && s.[0] = '[' then (
        match String.index_opt s ']' with
          | Some i ->
              drop
                (String.trim (String.sub s (i + 1) (String.length s - i - 1)))
          | None -> s)
      else s
    in
    drop s
  in
  List.concat_map
    (fun chain ->
      List.filter_map
        (fun filter ->
          let filter = strip_labels filter in
          let stop =
            List.fold_left
              (fun stop c ->
                match String.index_opt filter c with
                  | Some i -> min stop i
                  | None -> stop)
              (String.length filter) ['='; '@'; '['; ' ']
          in
          match String.sub filter 0 stop with "" -> None | name -> Some name)
        (String.split_on_char ',' chain))
    (String.split_on_char ';' description)

let endpoints = ["abuffer"; "buffer"; "abuffersink"; "buffersink"]

(* §15.2. *)
let parse t ~description ~(audio_inputs : (string * [ `Audio ] D.pad) list)
    ~(video_inputs : (string * [ `Video ] D.pad) list) ~audio_outputs
    ~video_outputs =
  (match
     List.find_opt
       (fun f -> List.mem f endpoints)
       (description_filters description)
   with
    | Some name ->
        failwith
          (Printf.sprintf
             "Filter %s cannot be used in a description: graph inputs and \
              outputs are the graph's only endpoints"
             name)
    | None -> ());
  let passthrough filter label =
    (label, D.element t.description ("parsed_" ^ filter))
  in
  let audio_out = List.map (passthrough "aformat") audio_outputs in
  let video_out = List.map (passthrough "format") video_outputs in
  let attach filter config (_, (e : D.element)) =
    e.attached <-
      Some (Avfilter.attach ~name:e.name (Avfilter.find filter) config)
  in
  D.add t.description (fun _ config ->
      List.iter (attach "aformat" config) audio_out;
      List.iter (attach "format" config) video_out;
      let node label pad =
        { Avfilter.node_name = label; node_args = None; node_pad = pad }
      in
      Avfilter.parse
        {
          inputs =
            {
              audio =
                List.map
                  (fun (l, e) ->
                    node l (List.hd (D.attached e).io.inputs.audio))
                  audio_out;
              video =
                List.map
                  (fun (l, e) ->
                    node l (List.hd (D.attached e).io.inputs.video))
                  video_out;
            };
          outputs =
            {
              audio =
                List.map
                  (fun (l, (p : _ D.pad)) -> node l (p.resolve ()))
                  audio_inputs;
              video =
                List.map
                  (fun (l, (p : _ D.pad)) -> node l (p.resolve ()))
                  video_inputs;
            };
        }
        description config);
  let given =
    List.map (fun (_, (p : _ D.pad)) -> p.element) audio_inputs
    @ List.map (fun (_, (p : _ D.pad)) -> p.element) video_inputs
  in
  List.iter
    (fun (_, (p : _ D.pad)) ->
      D.consumed t.description `Output p.element p.media p.index)
    audio_inputs;
  List.iter
    (fun (_, (p : _ D.pad)) ->
      D.consumed t.description `Output p.element p.media p.index)
    video_inputs;
  let returned media out =
    List.map
      (fun (label, (e : D.element)) ->
        List.iter (fun src -> D.edge t.description ~src ~dst:e) given;
        D.expect t.description e `Output media 0 label;
        (label, e))
      out
  in
  ( List.map
      (fun (label, e) -> (label, output_pad t e audio))
      (returned `Audio audio_out),
    List.map
      (fun (label, e) -> (label, output_pad t e video))
      (returned `Video video_out) )

(* §6.3. *)
let process_command t (e : D.element) ~fast cmd arg =
  match t.generation with
    | None -> "graph not started!"
    | Some _ -> (
        try
          locked t (fun () ->
              Avfilter.process_command
                ~flags:(if fast then [`Fast] else [])
                ~cmd ~arg (D.attached e))
        with Avutil.Error err ->
          let message = Avutil.string_of_error err in
          t.log#important "Command %s refused by %s: %s" cmd e.name message;
          message)

(* §7.2, steps 3 to 5. *)
let close t ~pos =
  (match D.first_unlinked t.description with
    | Some pad -> failwith (Printf.sprintf "Unconnected %s" pad)
    | None -> ());
  let output_clock = Clock.create ~id:"ffmpeg.filter" () in
  let input_clock =
    Clock.create ~id:"ffmpeg.filter.input" ~sync:`Passive ~parent:output_clock
      ~owner:{ Clock.kind = "ffmpeg filter graph"; id = t.id }
      ()
  in
  t.output_clock <- Some output_clock;
  t.input_clock <- Some input_clock;
  List.iter (fun i -> Clock.unify ~pos i.i_operator#clock input_clock) t.inputs;
  Option.iter (fun s -> Clock.unify ~pos s#clock output_clock) t.source;
  let reached =
    D.reached t.description ~from:(List.map (fun i -> i.i_element) t.inputs)
  in
  List.iter (fun s -> s.reached <- reached s.s_element) t.sinks;
  D.close t.description
