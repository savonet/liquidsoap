(* The output.file.hls operator (spec/operator.md): construction, lifecycle,
   the per-chunk loop and errors. Each step defers to the module that owns
   its rule: Segmenter, Window, Playlist, Persist, File_store, Variant. *)

open Variant

exception Fatal of exn

let invalid value message = raise (Error.Invalid_value (value, message, []))

type state = [ `Idle | `Running | `Waiting of float ]

(* From wake-up to sleep (spec/operator.md §3). [Restore_failed]: a strict
   restore failed, the state file is kept and never saved over until the next
   wake-up. *)
type life = Asleep | Restore_failed | Awake of { restored : bool; runs : int }

let is_playlist_tag tag =
  String.length tag > 0
  && tag.[0] = '#'
  && not (String.exists (fun c -> c = '\n' || c = '\r') tag)

let check_tags value tags =
  List.map
    (fun tag ->
      let tag = String.trim tag in
      if is_playlist_tag tag then tag
      else invalid value (Printf.sprintf "Not a playlist tag: %S" tag))
    tags

let seconds ticks = Frame.seconds_of_main ticks

let parse_fields value =
  let meths, _ = Lang.split_meths value in
  let get name f = Option.map f (List.assoc_opt name meths) in
  {
    Variant.bandwidth = get "bandwidth" Lang.to_int;
    codecs = get "codecs" Lang.to_string;
    extname = get "extname" Lang.to_string;
    video_size =
      get "video_size" (fun v ->
          let w, h = Lang.to_product v in
          (Lang.to_int w, Lang.to_int h));
    id3 = get "id3" Lang.to_bool;
    id3_version = get "id3_version" Lang.to_int;
    replay_id3 = Option.value ~default:true (get "replay_id3" Lang.to_bool);
    extra_tags =
      Option.value ~default:[]
        (get "extra_tags" (fun v ->
             check_tags v (List.map Lang.to_string (Lang.to_list v))));
  }

let build_variant ~pos (name, value) =
  let format_value = Lang.demeth value in
  let format = Lang.to_format format_value in
  let fields = parse_fields value in
  (try
     let (_ : Encoder.factory) = Encoder.get_factory format in
     ()
   with Not_found -> invalid value "Unsupported format");
  let extname =
    match fields.extname with
      | Some e -> e
      | None when Variant.iso_muxer format -> "mp4"
      | None -> (
          try Encoder.extension format
          with Not_found ->
            invalid value
              (Printf.sprintf "Variant %s: no file extension for this format"
                 name))
  in
  let factory =
    match Encoder.get_hls_factory format with
      | f -> f
      | exception Not_found ->
          invalid value
            (Printf.sprintf
               "Variant %s: transport of other media file formats is not \
                defined (RFC 8216 §3.1)"
               name)
  in
  let encoder = factory ~pos name in
  let id3_mode =
    match encoder.init ?id3:fields.id3 ?id3_version:fields.id3_version () with
      | Ok mode -> mode
      | Error reason ->
          invalid value (Printf.sprintf "Variant %s: %s" name reason)
  in
  (try ignore (encoder.hls_stop ()) with _ -> ());
  {
    Variant.name;
    format_value;
    format;
    fields;
    extname;
    id3_mode;
    factory;
    window = Window.create ();
    tags_lock = Mutex.create ();
    pending_tags = [];
    pending_discontinuity = false;
    current_init = None;
    inits = [];
    metadata = `None;
    run = None;
  }

let probe_writable value dir =
  try
    let name = Filename.concat dir (File_store.temp_prefix ^ "probe") in
    Unix.close
      (Unix.openfile name [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600);
    Unix.unlink name
  with Unix.Unix_error (e, _, _) ->
    invalid value
      (Printf.sprintf "Directory %s is not writable: %s" dir
         (Unix.error_message e))

let make_dir value ~perm dir =
  try Utils.mkdir ~perm dir
  with Unix.Unix_error (e, _, _) ->
    invalid value
      (Printf.sprintf "Cannot create directory %s: %s" dir
         (Unix.error_message e))

let device dir = try Some (Unix.stat dir).Unix.st_dev with _ -> None

let tag_meth insert =
  Lang.val_fun
    [("", "", None)]
    (fun p ->
      let tag = List.assoc "" p in
      (try insert (Lang.to_string tag)
       with Failure message ->
         Lang.raise_error ~message
           ~pos:(Value.pos tag |> Option.to_list)
           "invalid");
      Lang.unit)

type lookup =
  [ `Tracked of string * [ `Max_age of int | `Expires of int ]
  | `Forbidden
  | `Other ]

(* Written by the streaming thread, read by harbor handlers. *)
let registry : (string, < lookup : string -> lookup >) Hashtbl.t =
  Hashtbl.create 4

let registry_lock = Mutex.create ()
let registered f = Mutex.protect registry_lock (fun () -> f registry)

(* The variant's main values, for the main playlist writer. *)
type main_value = {
  name : string;
  bandwidth : int;
  codecs : string option;
  video_size : (int * int) option;
}

class output p =
  let source_val = Lang.assoc "" 3 p in
  let arg name = List.assoc name p in
  let dir_value = Lang.assoc "" 1 p in
  let variants_value = Lang.assoc "" 2 p in
  let infallible = not (Lang.to_bool (arg "fallible")) in
  let autostart = Lang.to_bool (arg "start") in
  let register_telnet = Lang.to_bool (arg "register_telnet") in
  let perm = Lang.to_int (arg "perm") in
  let dir_perm = Lang.to_int (arg "dir_perm") in
  let dir = Lang_string.home_unrelate (Lang.to_string dir_value) in
  let () = make_dir dir_value ~perm:dir_perm dir in
  let () = probe_writable dir_value dir in
  let temp_dir =
    Option.map
      (fun d -> Lang_string.home_unrelate d)
      (Lang.to_valued_option Lang.to_string (arg "temp_dir"))
  in
  let () =
    Option.iter
      (fun d ->
        make_dir (arg "temp_dir") ~perm:dir_perm d;
        probe_writable (arg "temp_dir") d)
      temp_dir
  in
  let persist_at =
    Option.map
      (fun f ->
        let f = Lang_string.home_unrelate f in
        if Filename.is_relative f then Filename.concat dir f else f)
      (Lang.to_valued_option Lang.to_string (arg "persist_at"))
  in
  let () =
    Option.iter
      (fun f ->
        let d = Filename.dirname f in
        make_dir (arg "persist_at") ~perm:dir_perm d;
        probe_writable (arg "persist_at") d)
      persist_at
  in
  let strict_persist = Lang.to_bool (arg "strict_persist") in
  let segment_duration_value = arg "segment_duration" in
  let frame = Lazy.Mutexed.force Frame.size in
  let duration =
    let ticks = Frame.main_of_seconds (Lang.to_float segment_duration_value) in
    ticks / frame * frame
  in
  let () =
    if duration < frame then
      invalid segment_duration_value
        "segment_duration is shorter than one frame"
  in
  let config =
    {
      Segmenter.duration;
      ticks_per_second = Lazy.Mutexed.force Frame.main_rate;
    }
  in
  let target = Segmenter.target_duration config in
  let max_segments = Lang.to_int (arg "segments") in
  let () =
    if max_segments < 1 then
      invalid (arg "segments") "segments must be at least 1"
  in
  let overhead = Lang.to_valued_option Lang.to_int (arg "segments_overhead") in
  let () =
    match overhead with
      | Some n when n < 0 ->
          invalid (arg "segments_overhead")
            "segments_overhead must not be negative"
      | _ -> ()
  in
  let extra_tags =
    check_tags (arg "extra_tags")
      (List.map Lang.to_string (Lang.to_list (arg "extra_tags")))
  in
  let prefix = Lang.to_string (arg "prefix") in
  let playlist = Lang.to_string (arg "playlist") in
  let cut_on_metadata =
    Lang.to_valued_option Lang.to_bool (arg "cut_on_metadata")
  in
  let segment_name_fn = arg "segment_name" in
  let main_playlist_writer = arg "main_playlist_writer" in
  let reopen_on_error = arg "reopen_on_error" in
  let pos = Value.pos variants_value in
  let variants =
    match Lang.to_list variants_value with
      | [] -> invalid variants_value "No variant given"
      | l ->
          let l =
            List.map
              (fun v ->
                let name, format = Lang.to_product v in
                (Lang.to_string name, format))
              l
          in
          let names = List.map fst l in
          if List.mem "" names then
            invalid variants_value "Variant names must not be empty";
          if List.length (List.sort_uniq compare names) <> List.length names
          then invalid variants_value "Variant names must be unique";
          List.map (build_variant ~pos) l
  in
  let () =
    if
      List.exists
        (fun v -> Variant.media_playlist v = File_store.canonical playlist)
        variants
    then
      invalid (arg "playlist")
        "The main playlist has the name of a variant's media playlist"
  in
  let state_names =
    let prefix = Filename.concat dir "" in
    match persist_at with
      | Some f when String.starts_with ~prefix f ->
          let rel =
            File_store.canonical
              (String.sub f (String.length prefix)
                 (String.length f - String.length prefix))
          in
          [rel; rel ^ ".tmp"]
      | _ -> []
  in
  let reserved =
    (File_store.canonical playlist :: List.map Variant.media_playlist variants)
    @ state_names
  in
  object (self)
    inherit
      [unit] Output.encoded
        ~output_kind:"output.file.hls" ~infallible ~register_telnet ~autostart
          ~export_cover_metadata:false ~name:"output.file.hls" source_val as base

    val on_file_change : (Lang.value -> unit) Callbacks.t = Callbacks.create ()
    method register_on_file_change fn = Callbacks.register on_file_change fn
    val mutable store = None

    method private store =
      match store with
        | Some s -> s
        | None ->
            let s =
              File_store.create ~dir ~temp_dir ~perm ~dir_perm ~reserved
                ~on_event:(fun ~state ~path ->
                  let record =
                    Lang.record
                      [("state", Lang.string state); ("path", Lang.string path)]
                  in
                  List.iter
                    (fun fn -> fn record)
                    (Callbacks.elements on_file_change))
                ()
            in
            store <- Some s;
            s

    val mutable state : state = `Idle
    val mutable stopping = false
    val mutable life = Asleep
    val mutable main = `Due
    val mutable pending_metadata = None
    val tag_cut = Atomic.make `None
    method private self_sync = (Lang.to_source source_val)#cached_self_sync
    method variants = variants
    method files = self#store

    initializer
      (match temp_dir with
        | Some d when device d <> device dir ->
            self#log#important
              "temp_dir is on another file system than the output directory: \
               every file is copied into place."
        | _ -> ());
      if max_segments < 6 then
        self#log#important
          "segments is below 6: Apple's authoring specification requires six \
           segments in a live playlist.";
      self#on_sleep (fun () -> self#final_stop)

    method private segment_name v ~position ~extname ~ticks =
      let record =
        Lang.record
          [
            ("position", Lang.int position);
            ("extname", Lang.string extname);
            ("duration", Lang.float (seconds ticks));
            ("ticks", Lang.int ticks);
            ("stream_name", Lang.string v.Variant.name);
          ]
      in
      let name = Lang.to_string (Lang.apply segment_name_fn [("", record)]) in
      if not (File_store.valid_name self#store name) then
        raise
          (Fatal
             (Failure
                (Printf.sprintf "Invalid segment name %S for variant %s" name
                   v.Variant.name)));
      name

    (* spec/files.md §6. *)
    method private set_init v init =
      match init with
        | Some "" ->
            self#log#important "Variant %s: empty init segment, ignored"
              v.Variant.name;
            v.current_init <- None
        | None -> v.current_init <- None
        | Some bytes ->
            let position = Window.next_init_position v.window in
            let name =
              self#segment_name v ~position ~extname:(Variant.init_extension v)
                ~ticks:0
            in
            File_store.write_now self#store ~name (`Init v.name)
              (Strings.of_string bytes);
            v.current_init <- Some name;
            v.inits <- v.inits @ [name]

    (* The encoder's first init segment, at [init_encode] or at its stop: a
       stopped encoder may no longer answer its properties. *)
    method private apply_init v r (ready : Encoder.hls_ready) =
      (try r.Variant.properties <- Some (Variant.read_properties r.encoder)
       with _ when r.encoder_stopped -> ());
      self#set_init v ready.init;
      (match Segmenter.current r.segmenter with
        | Some s -> s.init <- v.current_init
        | None -> ());
      ready.pieces

    method private open_segment v ~start =
      let run = Option.get v.Variant.run in
      let segment =
        {
          Variant.start;
          discontinuous = v.pending_discontinuity;
          tags = Variant.take_tags v;
          init = v.current_init;
          first_id3 = None;
        }
      in
      v.pending_discontinuity <- false;
      let first_id3 =
        match (v.id3_mode, v.metadata) with
          | `Prepended, `Pending m ->
              v.metadata <- `Sent m;
              Some m
          | _ -> None
      in
      let segment = { segment with first_id3 } in
      let id3 =
        match v.id3_mode with
          | `None -> None
          | `Prepended ->
              let m =
                match (first_id3, v.metadata, v.fields.replay_id3) with
                  | Some m, _, _ | None, `Sent m, true -> m
                  | _ -> Frame.Metadata.Export.empty
              in
              run.encoder.insert_id3 ~time:start m
          | `In_stream ->
              (match (v.metadata, v.fields.replay_id3, run.last_tag) with
                | `Sent m, true, last
                  when Option.fold ~none:true ~some:(fun l -> start > l) last ->
                    ignore (run.encoder.insert_id3 ~time:start m);
                    run.last_tag <- Some start
                | _ -> ());
              None
      in
      ( segment,
        match id3 with
          | Some tag -> Strings.of_string tag
          | None -> Strings.empty )

    method private close_segment v (closed : Variant.segment Segmenter.closed) =
      let segment = closed.payload in
      let discontinuous = segment.discontinuous || closed.discontinuous in
      if not closed.publishable then Variant.give_back v segment ~discontinuous
      else (
        (match v.run with
          | Some ({ properties = Some _; encoder_stopped = false; _ } as r) -> (
              try r.properties <- Some (Variant.read_properties r.encoder)
              with _ -> ())
          | _ -> ());
        self#publish v segment closed ~discontinuous)

    (* spec/files.md §4. *)
    method private publish v segment closed ~discontinuous =
      let name, fire =
        try
          let name =
            self#segment_name v ~position:v.Variant.window.next_position
              ~extname:(Variant.segment_extension v)
              ~ticks:closed.length
          in
          ( name,
            File_store.write self#store ~name (`Segment v.name) closed.bytes )
        with exn ->
          Variant.give_back v segment ~discontinuous;
          raise exn
      in
      let codecs = Variant.encoder_answer v (fun p -> p.codecs) in
      ignore
        (Window.publish v.window ~discontinuous ~forced:closed.forced ~codecs
           ~init:segment.init ~name ~tags:segment.tags
           ~duration:(seconds closed.length)
           ~size:(Strings.length closed.bytes));
      let failure = ref None in
      let step f =
        try f ()
        with exn ->
          let bt = Printexc.get_raw_backtrace () in
          if !failure = None then failure := Some (exn, bt)
      in
      step (fun () -> self#save);
      step fire;
      step (fun () -> self#write_media_playlist v);
      step (fun () -> if not stopping then self#write_main_playlist);
      step (fun () -> self#retention v);
      Option.iter
        (fun (exn, bt) -> Printexc.raise_with_backtrace exn bt)
        !failure

    method private write_media_playlist v =
      Window.slide v.Variant.window ~max_segments ~target;
      let listed = Window.listed v.window in
      let uri = Playlist.uri ~prefix in
      let entries =
        List.map
          (fun (s : Window.segment) ->
            {
              Playlist.uri = uri s.name;
              duration = s.duration;
              discontinuous = s.discontinuous;
              init_uri = Option.map uri s.init;
              tags = s.tags;
            })
          listed
      in
      let independent =
        Option.value ~default:false
          (Variant.encoder_answer v (fun p -> Some p.independent))
        && not (List.exists (fun (s : Window.segment) -> s.forced) listed)
      in
      let bytes =
        Playlist.media
          {
            target;
            independent;
            media_sequence = Window.media_sequence v.window;
            discontinuity_sequence = Window.discontinuity_sequence v.window;
            extra_tags = v.fields.extra_tags;
            entries;
          }
      in
      File_store.write_now self#store ~name:(Variant.media_playlist v) `Playlist
        (Strings.of_string bytes);
      Window.mark_listed v.window

    method private retention v =
      let expired = Window.expire v.Variant.window ~overhead in
      if overhead <> None then (
        List.iter
          (fun (s : Window.segment) -> File_store.delete self#store s.name)
          expired;
        self#init_retention v)

    method private init_retention v =
      let referenced =
        Option.to_list v.current_init
        @ Window.referenced_inits v.window
        @
          match v.run with
          | Some r -> (
              match Segmenter.current r.segmenter with
                | Some s -> Option.to_list s.init
                | None -> [])
          | None -> []
      in
      let unused, used =
        List.partition (fun i -> not (List.mem i referenced)) v.inits
      in
      v.inits <- used;
      List.iter (File_store.delete self#store) unused

    method private variant_value v =
      let bandwidth = Variant.bandwidth v ~target in
      let codecs = Variant.codecs v in
      let video_size = Variant.video_size v in
      { name = v.Variant.name; bandwidth; codecs; video_size }

    (* spec/playlists.md §3. *)
    method private write_main_playlist =
      if List.for_all (fun v -> Window.listed v.Variant.window <> []) variants
      then (
        let values = List.map self#variant_value variants in
        List.iter2
          (fun v { bandwidth = announced; codecs; _ } ->
            if codecs = None then
              self#log#important
                "Variant %s: no codec string, CODECS is left out" v.Variant.name;
            match (v.run, Variant.measured v ~target) with
              | Some r, Some m when m > announced && not r.bandwidth_warned ->
                  r.bandwidth_warned <- true;
                  self#log#important
                    "Variant %s: measured bandwidth %d exceeds the announced %d"
                    v.name m announced
              | _ -> ())
          variants values;
        if main <> `Written values then (
          main <- `Written values;
          match Lang.to_valued_option Fun.id main_playlist_writer with
            | None -> ()
            | Some writer -> (
                let streams =
                  List.map
                    (fun { name; bandwidth; codecs; video_size } ->
                      Lang.meth (Lang.string name)
                        ([
                           ("bandwidth", Lang.int bandwidth);
                           ( "codecs",
                             Lang.string (Option.value ~default:"" codecs) );
                         ]
                        @
                          match video_size with
                          | Some (w, h) ->
                              [
                                ( "video_size",
                                  Lang.product (Lang.int w) (Lang.int h) );
                              ]
                          | None -> []))
                    values
                in
                let result =
                  Lang.apply writer
                    [
                      ("extra_tags", Lang.list (List.map Lang.string extra_tags));
                      ("prefix", Lang.string prefix);
                      ("version", Lang.int Playlist.version);
                      ("", Lang.list streams);
                    ]
                in
                match Lang.to_valued_option Lang.to_string result with
                  | Some content ->
                      File_store.write_now self#store ~name:playlist `Playlist
                        (Strings.of_string content)
                  | None -> ())))

    method private persisted =
      List.map
        (fun v ->
          let w = v.Variant.window in
          {
            Persist.name = v.Variant.name;
            next_position = w.next_position;
            next_init_position = w.next_init_position;
            discontinuity_count = w.discontinuity_count;
            window_start = w.window_start;
            pending_tags = Variant.pending_tags v;
            segments = Window.to_persisted w;
          })
        variants

    (* spec/persistence.md §3. *)
    method private save =
      match persist_at with
        | Some file when self#awake -> (
            try
              let temp = file ^ ".tmp" in
              let oc = open_out_bin temp in
              Fun.protect
                ~finally:(fun () -> close_out oc)
                (fun () -> output_string oc (Persist.to_string self#persisted));
              Unix.rename temp file
            with exn ->
              self#log#important "Could not save the state to %s: %s" file
                (Printexc.to_string exn))
        | _ -> ()

    method private awake = match life with Awake _ -> true | _ -> false

    (* spec/persistence.md §4. *)
    method private restore =
      match persist_at with
        | Some file when Sys.file_exists file -> (
            let content =
              try Ok (Utils.read_all file)
              with exn -> Error (Printexc.to_string exn)
            in
            let parsed =
              Result.bind content
                (Persist.of_string
                   ~names:(List.map (fun v -> v.Variant.name) variants)
                   ~valid_name:(File_store.allowed self#store))
            in
            match parsed with
              | Error reason when strict_persist ->
                  life <- Restore_failed;
                  raise
                    (Fatal
                       (Failure
                          (Printf.sprintf "Invalid state file %s: %s" file
                             reason)))
              | Error reason ->
                  self#log#important "Ignoring the state file %s: %s" file
                    reason;
                  false
              | Ok saved ->
                  List.iter
                    (fun (s : Persist.variant) -> self#apply_saved s)
                    saved;
                  true)
        | _ -> false

    method private apply_saved (saved : Persist.variant) =
      let v = List.find (fun v -> v.Variant.name = saved.name) variants in
      let path = File_store.path self#store in
      let present (s : Persist.segment) =
        Sys.file_exists (path s.name)
        && Option.fold ~none:true
             ~some:(fun i -> Sys.file_exists (path i))
             s.init
      in
      let segments =
        match
          List.rev (List.filter (fun s -> not (present s)) saved.segments)
        with
          | [] -> saved.segments
          | newest :: _ ->
              self#log#important
                "Variant %s: %s is missing, dropping it and every older segment"
                v.name newest.Persist.name;
              List.filter
                (fun (s : Persist.segment) -> s.position > newest.position)
                saved.segments
      in
      let window_start =
        match segments with
          | s :: _ -> max saved.window_start s.Persist.position
          | [] -> saved.next_position
      in
      let sized =
        List.map
          (fun (s : Persist.segment) ->
            (s, (Unix.stat (path s.name)).Unix.st_size))
          segments
      in
      v.window <-
        Window.restore ~next_position:saved.next_position
          ~next_init_position:saved.next_init_position
          ~discontinuity_count:saved.discontinuity_count ~window_start sized;
      Variant.give_back_tags v saved.pending_tags;
      List.iter
        (fun (s : Persist.segment) ->
          File_store.track self#store s.name (`Segment v.name);
          Option.iter
            (fun i ->
              if not (List.mem i v.inits) then (
                File_store.track self#store i (`Init v.name);
                v.inits <- v.inits @ [i]))
            s.init)
        segments

    (* spec/operator.md §3.1 steps 2–4. *)
    method private begin_run =
      List.iter
        (fun v ->
          let encoder = v.Variant.factory ~pos v.name in
          (match
             encoder.init ?id3:v.fields.id3 ?id3_version:v.fields.id3_version ()
           with
            | Ok _ -> ()
            | Error reason -> failwith reason);
          v.run <-
            Some
              {
                Variant.encoder;
                segmenter =
                  Segmenter.create
                    ~on_open:(fun ~start -> self#open_segment v ~start)
                    ~on_close:(fun c -> self#close_segment v c)
                    config;
                properties = None;
                encoder_stopped = false;
                last_tag = None;
                bandwidth_warned = false;
              };
          v.metadata <- `None;
          v.current_init <- None;
          match life with
            | Awake { restored = false; runs = 0 } -> ()
            | _ -> v.pending_discontinuity <- true)
        variants;
      (match life with
        | Awake ({ runs; _ } as l) -> life <- Awake { l with runs = runs + 1 }
        | Asleep | Restore_failed -> ());
      main <- `Due;
      state <- `Running

    method private start =
      registered (fun r ->
          Hashtbl.replace r self#id (self :> < lookup : string -> lookup >));
      (match life with
        | Asleep | Restore_failed ->
            let restored = self#restore in
            life <- Awake { restored; runs = 0 };
            if restored then List.iter self#write_media_playlist variants
        | Awake _ -> ());
      self#begin_run

    (* spec/operator.md §3.2. *)
    method private stop =
      let running = List.exists (fun v -> v.Variant.run <> None) variants in
      stopping <- true;
      Fun.protect
        ~finally:(fun () -> stopping <- false)
        (fun () ->
          List.iter
            (fun v ->
              match v.Variant.run with
                | None -> ()
                | Some r -> (
                    (try
                       let ready = r.encoder.hls_stop () in
                       r.encoder_stopped <- true;
                       let pieces =
                         if r.properties = None then self#apply_init v r ready
                         else ready.pieces
                       in
                       if Segmenter.current r.segmenter <> None then
                         self#apply_pieces v pieces
                     with exn ->
                       r.encoder_stopped <- true;
                       self#log#important
                         "Variant %s: stopping the encoder failed: %s" v.name
                         (Printexc.to_string exn));
                    try Segmenter.finish r.segmenter
                    with exn ->
                      self#log#important
                        "Variant %s: closing the last segment failed: %s" v.name
                        (Printexc.to_string exn)))
            variants);
      (try if running then self#write_main_playlist
       with exn ->
         self#log#important "Writing the main playlist failed: %s"
           (Printexc.to_string exn));
      List.iter (fun v -> v.Variant.run <- None) variants;
      if state = `Running then state <- `Idle;
      self#save

    method private abort_run =
      List.iter
        (fun v ->
          (match v.Variant.run with
            | Some r -> (
                match Segmenter.abort r.segmenter with
                  | Some s ->
                      Variant.give_back v s ~discontinuous:s.discontinuous
                  | None -> ())
            | None -> ());
          v.run <- None)
        variants

    (* spec/files.md §7. *)
    method private final_stop =
      (try self#stop with _ -> ());
      if persist_at = None then (
        self#abort_run;
        let delete kind_filter =
          List.iter
            (fun (name, kind) ->
              if kind_filter kind then File_store.delete self#store name)
            (File_store.tracked self#store)
        in
        (try File_store.delete self#store playlist with _ -> ());
        delete (fun k -> k = `Playlist);
        delete (function `Segment _ -> true | _ -> false);
        delete (function `Init _ -> true | _ -> false));
      List.iter
        (fun v ->
          v.Variant.window <- Window.create ();
          v.inits <- [];
          ignore (Variant.take_tags v);
          v.pending_discontinuity <- false)
        variants;
      registered (fun r -> Hashtbl.remove r self#id);
      life <- Asleep;
      store <- None;
      state <- `Idle

    method! output =
      match state with
        | `Waiting date when Unix.gettimeofday () < date -> ()
        | _ -> (
            try base#output with
              | Fatal exn ->
                  self#abort_run;
                  raise exn
              | exn ->
                  let bt = Printexc.get_raw_backtrace () in
                  self#abort_run;
                  state <-
                    `Waiting
                      (Pipe_output.reopen_time_on_error ~log:self#log
                         reopen_on_error ~bt exn))

    method private all_initialized = List.for_all Variant.initialized variants

    method private alignment =
      List.length variants = 1
      || List.for_all
           (fun v ->
             Option.value ~default:false
               (Variant.encoder_answer v (fun p -> Some p.free_split)))
           variants

    method private sync_all ~time ~discontinuous =
      List.iter
        (fun v ->
          Option.iter
            (fun r -> Segmenter.sync r.Variant.segmenter ~time ~discontinuous)
            v.Variant.run)
        variants

    method insert_tag ?variant tag =
      let tag = String.trim tag in
      if not (is_playlist_tag tag) then
        failwith (Printf.sprintf "Not a playlist tag: %S" tag);
      (match variant with
        | Some v -> Variant.add_tag v tag
        | None -> List.iter (fun v -> Variant.add_tag v tag) variants);
      Atomic.set tag_cut (match variant with Some _ -> `One | None -> `All)

    method private encode_metadata m = pending_metadata <- Some m

    (* spec/metadata.md §1 and §4. *)
    method private take_metadata ~time =
      match pending_metadata with
        | None -> []
        | Some m ->
            pending_metadata <- None;
            let changed =
              List.filter
                (fun v ->
                  match v.Variant.metadata with
                    | `Sent m' when m' = m -> false
                    | _ ->
                        v.metadata <- `Pending m;
                        true)
                variants
            in
            if changed <> [] && self#all_initialized then (
              let cut =
                match cut_on_metadata with
                  | Some c -> c
                  | None ->
                      self#alignment
                      && List.exists
                           (fun v -> v.Variant.id3_mode <> `None)
                           variants
              in
              if cut then self#sync_all ~time ~discontinuous:false);
            List.filter (fun v -> v.Variant.id3_mode = `In_stream) changed

    method private take_tag_cut ~time =
      match Atomic.exchange tag_cut `None with
        | `None -> ()
        | scope ->
            if
              self#all_initialized
              && (scope = `All || List.length variants = 1)
              &&
                match cut_on_metadata with
                | Some c -> c
                | None -> self#alignment
            then self#sync_all ~time ~discontinuous:false

    method private apply_pieces v pieces =
      let r = Option.get v.Variant.run in
      List.iter
        (function
          | Encoder.Bytes b -> Segmenter.add_bytes r.segmenter b
          | Encoder.Split { time; forced } ->
              if forced then
                self#log#important
                  "Variant %s: forced split without a key packet at %.3fs"
                  v.name (seconds time);
              Segmenter.split r.segmenter ~time ~forced
          | Encoder.Restart { time; init } ->
              self#set_init v init;
              Segmenter.restart r.segmenter ~time;
              r.properties <- Some (Variant.read_properties r.encoder);
              List.iter
                (fun other ->
                  if other != v then
                    Option.iter
                      (fun r ->
                        Segmenter.sync r.Variant.segmenter ~time
                          ~discontinuous:true)
                      other.Variant.run)
                variants)
        pieces

    (* spec/operator.md §4, for one chunk. *)
    method private encode chunk =
      let length = Frame.position chunk in
      if length > 0 then (
        if state <> `Running then self#begin_run;
        let time =
          match (List.hd variants).run with
            | Some r -> Segmenter.media_time r.segmenter
            | None -> 0
        in
        let in_stream = self#take_metadata ~time in
        self#take_tag_cut ~time;
        let pieces =
          List.map
            (fun v ->
              let r = Option.get v.Variant.run in
              let split_due = r.properties <> None && r.encoder.split_due () in
              let force = Segmenter.bound ~split_due ~length r.segmenter in
              let boundary = Segmenter.request r.segmenter in
              if r.properties <> None then
                r.encoder.split_encode ~boundary ~force chunk
              else (
                match r.encoder.init_encode ~boundary chunk with
                  | Encoder.Pending -> []
                  | Encoder.Init ready -> self#apply_init v r ready))
            variants
        in
        List.iter2
          (fun v pieces ->
            let r = Option.get v.Variant.run in
            Segmenter.advance r.segmenter length;
            self#apply_pieces v pieces)
          variants pieces;
        List.iter
          (fun v ->
            match (v.Variant.metadata, v.run) with
              | `Pending m, Some r ->
                  ignore (r.encoder.insert_id3 ~time m);
                  r.last_tag <- Some time;
                  v.metadata <- `Sent m
              | _ -> ())
          in_stream)

    method streams =
      List.map
        (fun v ->
          Lang.record
            [
              ("name", Lang.string v.Variant.name);
              ("encoder", v.format_value);
              ( "video_size",
                match Variant.video_size v with
                  | Some (w, h) ->
                      Lang.record
                        [("width", Lang.int w); ("height", Lang.int h)]
                  | None -> Lang.null );
              ("bandwidth", Lang.int (Variant.bandwidth v ~target));
              ( "codecs",
                Lang.string (Option.value ~default:"" (Variant.codecs v)) );
              ("extname", Lang.string (Variant.segment_extension v));
              ("id3_enabled", Lang.bool (v.id3_mode <> `None));
              ("replay_id3", Lang.bool v.fields.replay_id3);
              ( "extra_tags",
                Lang.list (List.map Lang.string v.fields.extra_tags) );
              ("discontinuity_count", Lang.int v.window.discontinuity_count);
              ( "insert_tag",
                tag_meth (fun tag -> self#insert_tag ~variant:v tag) );
            ])
        variants

    (* spec/serving.md §3 and §4: what a request path names. *)
    method lookup rel =
      let rel = File_store.canonical rel in
      let forbidden =
        String.contains rel '\000'
        || List.mem ".." (String.split_on_char '/' rel)
        || String.starts_with ~prefix:File_store.temp_prefix
             (Filename.basename rel)
        || List.mem rel state_names
      in
      match Option.bind store (fun s -> File_store.kind s rel) with
        | Some `Playlist ->
            `Tracked (Content_type.playlist, `Max_age (target / 2))
        | Some (`Segment name) ->
            let v = List.find (fun (v : Variant.t) -> v.name = name) variants in
            let lifetime =
              match
                List.find_opt
                  (fun (s : Window.segment) -> s.name = rel)
                  v.window.segments
              with
                | Some seg -> Window.lifetime v.window seg
                | None -> 0.
            in
            `Tracked
              ( Content_type.segment v,
                `Expires (int_of_float (Float.ceil lifetime)) )
        | Some (`Init name) ->
            let v = List.find (fun (v : Variant.t) -> v.name = name) variants in
            let lifetime = Window.duration (Window.listed v.window) in
            `Tracked
              ( Content_type.init v,
                `Expires (int_of_float (Float.ceil lifetime)) )
        | None -> if forbidden then `Forbidden else `Other

    method private send () = ()
    method! private reset = ()
  end

let fields_t =
  [
    ("bandwidth", ([], Lang.int_t), "Announced bandwidth, in bits per second.");
    ("codecs", ([], Lang.string_t), "Announced codec string (RFC 6381).");
    ("extname", ([], Lang.string_t), "Media segment extension.");
    ( "video_size",
      ([], Lang.product_t Lang.int_t Lang.int_t),
      "Announced resolution." );
    ("id3", ([], Lang.bool_t), "ID3 on or off.");
    ("id3_version", ([], Lang.int_t), "ID3v2 version.");
    ( "replay_id3",
      ([], Lang.bool_t),
      "Repeat the current metadata at each segment." );
    ( "extra_tags",
      ([], Lang.list_t Lang.string_t),
      "Tags for this variant's media playlist." );
  ]

let segment_name_t =
  Lang.fun_t
    [
      ( false,
        "",
        Lang.record_t
          [
            ("position", Lang.int_t);
            ("extname", Lang.string_t);
            ("duration", Lang.float_t);
            ("ticks", Lang.int_t);
            ("stream_name", Lang.string_t);
          ] );
    ]
    Lang.string_t

let stream_t =
  Lang.optional_method_t
    (Lang.method_t Lang.string_t
       [
         ("bandwidth", ([], Lang.int_t), "Bandwidth, in bits per second.");
         ("codecs", ([], Lang.string_t), "Codec string, empty when unknown.");
       ])
    [("video_size", ([], Lang.product_t Lang.int_t Lang.int_t), "Video size.")]

let main_playlist_writer_t =
  Lang.fun_t
    [
      (false, "extra_tags", Lang.list_t Lang.string_t);
      (false, "prefix", Lang.string_t);
      (false, "version", Lang.int_t);
      (false, "", Lang.list_t stream_t);
    ]
    (Lang.nullable_t Lang.string_t)

let default_segment_name =
  Lang.eval ~cache:false ~stdlib:`Disabled ~typecheck:false
    "fun (s) -> \"#{s.stream_name}_#{s.position}.#{s.extname}\""

let default_reopen_on_error =
  Lang.eval ~cache:false ~stdlib:`Disabled ~typecheck:false "fun (_) -> 2."

let proto frame_t =
  Output.proto
  @ [
      ( "playlist",
        Lang.string_t,
        Some (Lang.string "stream.m3u8"),
        Some "Main playlist file name." );
      ( "extra_tags",
        Lang.list_t Lang.string_t,
        Some (Lang.list []),
        Some "Tags for the main playlist." );
      ( "prefix",
        Lang.string_t,
        Some (Lang.string ""),
        Some "Prefix of every URI in the playlists." );
      ( "main_playlist_writer",
        Lang.nullable_t main_playlist_writer_t,
        Some Lang.null,
        Some "Writes the main playlist; `null` writes none." );
      ( "segment_duration",
        Lang.float_t,
        Some (Lang.float 10.),
        Some "Segment duration, in seconds." );
      ( "segment_name",
        segment_name_t,
        Some default_segment_name,
        Some "Segment file names." );
      ( "segments",
        Lang.int_t,
        Some (Lang.int 10),
        Some "Number of segments in each media playlist." );
      ( "segments_overhead",
        Lang.nullable_t Lang.int_t,
        Some (Lang.int 5),
        Some
          "Segments kept on disk after leaving the playlist; `null` keeps \
           every segment." );
      ( "cut_on_metadata",
        Lang.nullable_t Lang.bool_t,
        Some Lang.null,
        Some
          "Start a new segment on new metadata or an inserted tag. `null`: \
           when it keeps the variants aligned." );
      ( "perm",
        Lang.int_t,
        Some (Lang.int 0o666),
        Some "Permission of the files, up to umask." );
      ( "dir_perm",
        Lang.int_t,
        Some (Lang.int 0o777),
        Some "Permission of the directories, up to umask." );
      ( "temp_dir",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some "Directory of temporary files." );
      ( "persist_at",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some "File where the playlist state is kept across restarts." );
      ( "strict_persist",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Fail when the state file cannot be restored." );
      ( "reopen_on_error",
        Lang.fun_t
          [(false, "", Lang.nullable_t Lang.error_t)]
          (Lang.nullable_t Lang.float_t),
        Some default_reopen_on_error,
        Some "Delay before a new run after an error; `null` or negative: fail."
      );
      ("", Lang.string_t, None, Some "Output directory.");
      ( "",
        Lang.list_t
          (Lang.product_t Lang.string_t
             (Lang.optional_method_t (Lang.format_t frame_t) fields_t)),
        None,
        Some "Variants: name and format." );
      ("", Lang.source_t frame_t, None, None);
    ]

let stream_record_t =
  Lang.record_t
    [
      ("name", Lang.string_t);
      ("encoder", Lang.format_t (Lang.univ_t ()));
      ( "video_size",
        Lang.nullable_t
          (Lang.record_t [("width", Lang.int_t); ("height", Lang.int_t)]) );
      ("bandwidth", Lang.int_t);
      ("codecs", Lang.string_t);
      ("extname", Lang.string_t);
      ("id3_enabled", Lang.bool_t);
      ("replay_id3", Lang.bool_t);
      ("extra_tags", Lang.list_t Lang.string_t);
      ("discontinuity_count", Lang.int_t);
      ("insert_tag", Lang.fun_t [(false, "", Lang.string_t)] Lang.unit_t);
    ]

let file_change_t =
  Lang.record_t [("state", Lang.string_t); ("path", Lang.string_t)]

let output_hls =
  let frame_t = Lang.univ_t () in
  Lang.add_operator ~base:Pipe_output.output_file "hls" (proto frame_t)
    ~return_t:frame_t ~category:`Output
    ~descr:
      "Output the source stream as an HTTP Live Streaming (HLS) presentation."
    ~meth:
      (Start_stop.meth ()
      @ [
          {
            Lang.name = "insert_tag";
            scheme = ([], Lang.fun_t [(false, "", Lang.string_t)] Lang.unit_t);
            descr =
              "Insert a playlist tag before the next segment of every variant.";
            value = (fun (s : output) -> tag_meth (fun tag -> s#insert_tag tag));
          };
          {
            Lang.name = "streams";
            scheme = ([], Lang.fun_t [] (Lang.list_t stream_record_t));
            descr = "The variants, with their current properties.";
            value =
              (fun (s : output) ->
                Lang.val_fun [] (fun _ -> Lang.list s#streams));
          };
        ])
    ~callbacks:
      (Start_stop.output_callbacks ()
      @ [
          {
            Lang.name = "on_file_change";
            params = [];
            descr = "when a file is created, updated or deleted";
            register_deprecated_argument = false;
            arg_t = [(false, "", file_change_t)];
            register =
              (fun ~params:_ s fn ->
                s#register_on_file_change (fun r -> fn [("", r)]));
          };
        ])
    (fun p -> new output p)

let http_date t =
  let tm = Unix.gmtime t in
  Printf.sprintf "%s, %02d %s %d %02d:%02d:%02d GMT"
    [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |].(tm.Unix.tm_wday)
    tm.tm_mday
    [|
      "Jan";
      "Feb";
      "Mar";
      "Apr";
      "May";
      "Jun";
      "Jul";
      "Aug";
      "Sep";
      "Oct";
      "Nov";
      "Dec";
    |].(tm.tm_mon)
    (tm.tm_year + 1900) tm.tm_hour tm.tm_min tm.tm_sec

let lookup_t =
  Lang.record_t
    [
      ("kind", Lang.string_t);
      ("content_type", Lang.string_t);
      ("headers", Lang.list_t (Lang.product_t Lang.string_t Lang.string_t));
    ]

let _ =
  Lang.add_builtin ~base:output_hls "lookup" ~category:`Internet
    ~flags:[`Hidden]
    ~descr:
      "Internal: what an HLS output serves at a path relative to its directory."
    [
      ("", Lang.string_t, None, Some "Output id.");
      ("", Lang.string_t, None, None);
    ]
    lookup_t
    (fun p ->
      let id = Lang.to_string (Lang.assoc "" 1 p) in
      let rel = Lang.to_string (Lang.assoc "" 2 p) in
      let header (k, v) = Lang.product (Lang.string k) (Lang.string v) in
      let result kind content_type headers =
        Lang.record
          [
            ("kind", Lang.string kind);
            ("content_type", Lang.string content_type);
            ("headers", Lang.list (List.map header headers));
          ]
      in
      match registered (fun r -> Hashtbl.find_opt r id) with
        | None -> result "forbidden" "" []
        | Some o -> (
            match o#lookup rel with
              | `Forbidden -> result "forbidden" "" []
              | `Other -> result "other" "" []
              | `Tracked (content_type, `Max_age a) ->
                  result "tracked" content_type
                    [("Cache-Control", Printf.sprintf "max-age=%d" a)]
              | `Tracked (content_type, `Expires s) ->
                  result "tracked" content_type
                    [
                      ("Cache-Control", Printf.sprintf "max-age=%d" s);
                      ( "Expires",
                        http_date (Unix.gettimeofday () +. float_of_int s) );
                    ]))

let _ =
  Lang.add_builtin ~base:output_hls "uri" ~category:`Internet ~flags:[`Hidden]
    ~descr:"Internal: a playlist URI for a file name (spec/playlists.md §4)."
    [
      ("prefix", Lang.string_t, Some (Lang.string ""), None);
      ("", Lang.string_t, None, None);
    ]
    Lang.string_t
    (fun p ->
      Lang.string
        (Playlist.uri
           ~prefix:(Lang.to_string (List.assoc "prefix" p))
           (Lang.to_string (Lang.assoc "" 1 p))))
