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

open Mm

exception Unavailable

module Queue = Queues.Queue
module WeakQueue = Queues.WeakQueue

type streaming_state =
  [ `Pending | `Unavailable | `Ready of unit -> unit | `Done of Frame.t ]

type active = < id : string ; reset : unit ; output : unit >
type source_type = [ `Passive | `Active of active | `Output of active ]
type sync = [ `Auto | `CPU | `None ]

module SourceSync = Clock.MkSyncSource (struct
  type t = < id : string >

  let to_string s = Printf.sprintf "source(id=%s)" s#id
end)

(** {1 Sources} *)

(** Instrumentation. *)

type metadata = (int * Frame.metadata) list

type watcher = {
  wake_up :
    fallible:bool ->
    source_type:source_type ->
    id:string ->
    ctype:Frame.content_type ->
    clock_id:string ->
    unit;
  sleep : unit -> unit;
  generate_frame :
    start_time:float ->
    end_time:float ->
    length:int ->
    has_track_mark:bool ->
    metadata:metadata ->
    unit;
  before_streaming_cycle : unit -> unit;
  after_streaming_cycle : unit -> unit;
}

type position_callback = {
  mode : [ `Remaining | `Elapsed ];
  allow_partial : bool;
  position : unit -> int;
  on_position : pos:float -> Frame.metadata -> unit;
  mutable executed : bool;
}

type frame_callback = { before : bool; on_frame : unit -> unit }

type on_frame =
  [ `Metadata of Frame.metadata -> unit
  | `Track of Frame.metadata -> unit
  | `Position of position_callback
  | `Frame of frame_callback ]

let source_log = Log.make ["source"]
let finalise s = source_log#info "Source %s is collected." s#id

let check_sleep ~activations ~s =
 fun src ->
  if List.memq src (WeakQueue.elements activations) then (
    (s#log : Log.t)#critical
      "Unbalanced activations! %s was cleaned-up without calling #sleep for \
       %s! Please report to the developers."
      s#id src#id;
    s#sleep src)

let hash = Atomic.make 0

class virtual operator ?(stack = []) ?clock ~name sources =
  let frame_type = Type.var () in
  let clock = match clock with Some c -> c | None -> Clock.create ~stack () in
  object (self)
    (** Monitoring *)
    val mutable watchers = []

    (** Sources and their activations. *)
    val mutable sources : (Clock.activation option * operator) list =
      List.map (fun s -> (None, s)) sources

    method add_watcher w = watchers <- w :: watchers
    method private iter_watchers fn = List.iter fn watchers
    method clock = clock

    initializer
      List.iter
        (fun (_, s) -> Clock.unify ~pos:self#pos self#clock s#clock)
        sources;
      Clock.attach self#clock (self :> Clock.source)

    val stack = Unifier.make stack
    method stack = Unifier.deref stack

    method set_stack p =
      Unifier.set stack p;
      Clock.set_stack clock p

    method stack_unifier = stack
    method pos = match Unifier.deref stack with [] -> None | p :: _ -> Some p

    (** Logging and identification *)

    val mutable log = source_log
    method private create_log = log <- Log.make [self#id]
    method log = log
    val mutable id = Lang_string.generate_id ~category:"source" name
    method id = id
    val hash = Atomic.fetch_and_add hash 1
    method hash = hash

    method set_id ?(force = true) s =
      let s =
        Re.Pcre.substitute
          ~rex:(Re.Pcre.regexp "[ \t\n.]")
          ~subst:(fun _ -> "_")
          s
      in
      if force && s <> self#id then (
        id <- Lang_string.generate_id ~category:"source" s;

        (* Sometimes the ID is changed during initialization, in order to make it
         equal to the server name, which is only registered at initialization
         time in order to avoid bloating from unused sources. If the ID
         changes, and [log] has already been initialized, reset it. *)
        if log != source_log then self#create_log)

    val mutex = Mutex.create ()

    method private mutexify : 'a 'b. ('a -> 'b) -> 'a -> 'b =
      Mutex_utils.mutexify mutex

    method virtual fallible : bool
    method source_type : source_type = `Passive
    val mutable registered_commands = Queue.create ()

    method register_command ?usage ~descr name cmd =
      self#on_wake_up (fun () ->
          let ns = [self#id] in
          Server.add ~ns ?usage ~descr name cmd;
          Queue.push registered_commands (ns, name))

    initializer
      self#on_sleep (fun () ->
          Queue.flush_iter registered_commands (fun (ns, name) ->
              Server.remove ~ns name))

    method active =
      match self#source_type with
        | `Passive -> false
        | `Output _ | `Active _ -> true

    method virtual self_sync : Clock.self_sync
    val mutable self_sync_source = None

    method private self_sync_source =
      match self_sync_source with
        | None ->
            let s = SourceSync.make (self :> < id : string >) in
            self_sync_source <- Some s;
            s
        | Some s -> s

    method source_sync self_sync =
      if self_sync then Some self#self_sync_source else None

    (* Type describing the contents of the frame: this should be a record
       whose fields (audio, video, etc.) indicate the kind of contents we
       have (e.g. {audio : pcm}). *)
    method frame_type = frame_type

    (* Content type should not be computed before
       the source has been asked to wake up. *)
    val mutable content_type_computation_allowed = false

    method content_type_computation_allowed =
      content_type_computation_allowed <- true

    val mutable ctype = None
    method has_content_type = ctype <> None

    (* Content type. *)
    method content_type =
      match ctype with
        | Some ctype -> ctype
        | None ->
            if not content_type_computation_allowed then
              failwith
                (Printf.sprintf
                   "Early computation of source content-type detected for \
                    source %s on clock %s!"
                   self#id (Clock.id self#clock));
            self#log#debug "Assigning source content type for frame type: %s"
              (Type.to_string self#frame_type);
            let ct = Frame_type.content_type self#frame_type in
            self#log#debug "Content type: %s" (Frame.string_of_content_type ct);
            ctype <- Some ct;
            ct

    val mutable audio_channels = -1

    method private audio_channels =
      match audio_channels with
        | -1 ->
            let c =
              match
                Frame.Fields.find_opt Frame.Fields.audio self#content_type
              with
                | Some c when Content.Audio.is_format c ->
                    Content.Audio.channels_of_format c
                | Some c when Content_pcm_s16.is_format c ->
                    Content_pcm_s16.channels_of_format c
                | Some c when Content_pcm_f32.is_format c ->
                    Content_pcm_f32.channels_of_format c
                | _ -> raise Content.Invalid
            in
            audio_channels <- c;
            c
        | c -> c

    val mutable samplerate = -1.

    method private samplerate =
      match samplerate with
        | -1. ->
            let s = float_of_int (Lazy.force Frame.audio_rate) in
            samplerate <- s;
            s
        | s -> s

    val mutable video_dimensions = None

    method private video_dimensions =
      match video_dimensions with
        | None ->
            let dim =
              Content.Video.dimensions_of_format
                (Option.get
                   (Frame.Fields.find_opt Frame.Fields.video self#content_type))
            in
            video_dimensions <- Some dim;
            dim
        | Some dim -> dim

    val mutable on_wake_up = []
    method on_wake_up fn = on_wake_up <- on_wake_up @ [fn]

    initializer
      self#on_wake_up (fun () ->
          sources <-
            List.map
              (fun (a, s) ->
                assert (a = None);
                let a = s#wake_up (self :> Clock.source) in
                (Some a, s))
              sources;
          self#iter_watchers (fun w ->
              w.wake_up ~fallible:self#fallible ~source_type:self#source_type
                ~id:self#id ~ctype:self#content_type
                ~clock_id:(Clock.id self#clock)))

    val is_up : [ `False | `True | `Error ] Atomic.t = Atomic.make `False
    method is_up = Atomic.get is_up = `True
    val streaming_state : streaming_state Atomic.t = Atomic.make `Pending
    val activations : Clock.activation WeakQueue.t = WeakQueue.create ()
    method activations = WeakQueue.elements activations

    method wake_up src =
      let activation =
        object
          method id = src#id
        end
      in
      Gc.finalise (check_sleep ~activations ~s:self) activation;
      WeakQueue.push activations activation;
      if Atomic.compare_and_set is_up `False `True then (
        try
          self#content_type_computation_allowed;
          if log == source_log then self#create_log;
          source_log#info
            "Source %s gets up from %s with content type: %s and frame type: \
             %s."
            self#id src#id
            (Frame.string_of_content_type self#content_type)
            (Type.to_string self#frame_type);
          self#log#debug "Clock is %s." (Clock.id self#clock);
          self#log#important "Content type is %s."
            (Frame.string_of_content_type self#content_type);
          List.iter (fun fn -> fn ()) on_wake_up
        with exn ->
          Atomic.set is_up `Error;
          let bt = Printexc.get_raw_backtrace () in
          Utils.log_exception ~log
            ~bt:(Printexc.raw_backtrace_to_string bt)
            (Printf.sprintf "Error while starting source %s: %s!" self#id
               (Printexc.to_string exn));
          Printexc.raise_with_backtrace exn bt);
      activation

    val mutable on_sleep = []
    method on_sleep fn = on_sleep <- on_sleep @ [fn]

    method private actual_sleep =
      if Atomic.compare_and_set is_up `True `False then (
        source_log#info "Source %s gets down." self#id;
        List.iter (fun fn -> fn ()) on_sleep)

    method sleep (src : Clock.activation) =
      WeakQueue.filter_out activations (fun s -> s == src);
      match
        ( WeakQueue.length activations,
          Clock.started self#clock,
          Atomic.get streaming_state )
      with
        | 0, true, (`Ready _ | `Unavailable) ->
            Clock.after_tick self#clock (fun () -> self#actual_sleep)
        | 0, _, _ -> self#actual_sleep
        | _ -> ()

    initializer
      Gc.finalise finalise self;
      self#on_sleep (fun () ->
          sources <-
            List.map
              (fun (a, s) ->
                s#sleep (Option.get a);
                (None, s))
              sources;
          self#iter_watchers (fun w -> w.sleep ()))

    (** Streaming *)

    (* Number of maste ticks left in the current track: -1 means unknown, time unit
       is master tick. *)
    method virtual remaining : int
    val mutable elapsed = 0
    method elapsed = elapsed

    method duration =
      let r = self#remaining in
      let e = self#elapsed in
      if r < 0 || e < 0 then -1 else e + r

    method virtual effective_source : source

    method seek n =
      let s = self#effective_source in
      if (s :> < seek : int -> int >) == (self :> < seek : int -> int >) then 0
      else s#seek n

    method virtual private can_generate_frame : bool
    method virtual private generate_frame : Frame.t

    method is_ready =
      if self#is_up && Clock.started self#clock then (
        self#before_streaming_cycle;
        match Atomic.get streaming_state with
          | `Ready _ | `Done _ -> true
          | _ -> false)
      else false

    val mutable _cache = None
    val mutable consumed = 0
    val mutable on_before_streaming_cycle = []

    method on_before_streaming_cycle fn =
      on_before_streaming_cycle <- on_before_streaming_cycle @ [fn]

    initializer
      self#on_before_streaming_cycle (fun () ->
          self#iter_watchers (fun w -> w.before_streaming_cycle ()))

    val mutable on_after_streaming_cycle = []

    method on_after_streaming_cycle fn =
      on_after_streaming_cycle <- on_after_streaming_cycle @ [fn]

    initializer
      self#on_after_streaming_cycle (fun () ->
          self#iter_watchers (fun w -> w.after_streaming_cycle ()))

    (* This is the implementation of the main streaming logic. *)
    method private before_streaming_cycle =
      match Atomic.get streaming_state with
        | `Pending ->
            List.iter (fun fn -> fn ()) on_before_streaming_cycle;
            consumed <- 0;
            let cache = Option.value ~default:self#empty_frame _cache in
            let cache_pos = Frame.position cache in
            let size = Lazy.force Frame.size in
            let can_generate_frame = self#can_generate_frame in
            if cache_pos > 0 || can_generate_frame then
              Atomic.set streaming_state
                (`Ready
                   (fun () ->
                     let buf =
                       if can_generate_frame && cache_pos < size then
                         Frame.append cache self#instrumented_generate_frame
                       else cache
                     in
                     let buf_pos = Frame.position buf in
                     let buf =
                       if size < buf_pos then (
                         _cache <- Some (Frame.after buf size);
                         Frame.slice buf size)
                       else (
                         _cache <- None;
                         buf)
                     in
                     Atomic.set streaming_state (`Done buf)))
            else Atomic.set streaming_state `Unavailable;
            Clock.after_tick self#clock (fun () -> self#after_streaming_cycle)
        | _ -> ()

    method private after_streaming_cycle =
      (match (Atomic.get streaming_state, consumed) with
        | `Done buf, n when n < Frame.position buf ->
            let cache = Option.value ~default:self#empty_frame _cache in
            _cache <- Some (Frame.append (Frame.after buf n) cache)
        | _ -> ());
      List.iter (fun fn -> fn ()) on_after_streaming_cycle;
      Atomic.set streaming_state `Pending

    method peek_frame =
      match Atomic.get streaming_state with
        | `Pending | `Unavailable ->
            log#critical "source called while not ready!";
            raise Unavailable
        | `Ready fn ->
            fn ();
            self#peek_frame
        | `Done data -> data

    method get_partial_frame cb =
      let data = cb self#peek_frame in
      consumed <- max consumed (Frame.position data);
      data

    method consumed n = consumed <- max consumed n
    method get_frame = self#get_partial_frame (fun f -> f)

    method get_mutable_content field =
      let content = Frame.get self#get_frame field in
      (* Optimization is disabled for now. *)
      Content.copy content

    method get_mutable_frame field =
      Frame.set self#get_frame field (self#get_mutable_content field)

    method set_frame_data :
        'a.
        Frame.field ->
        (?offset:int -> ?length:int -> 'a -> Content.data) ->
        'a ->
        Frame.t =
      fun field lift data -> Frame.set_data self#get_frame field lift data

    method private split_frame frame =
      match Frame.track_marks frame with
        | 0 :: _ -> (self#empty_frame, Some frame)
        | p :: _ -> (Frame.slice frame p, Some (Frame.after frame p))
        | [] -> (frame, None)

    method frame_has_track_mark = Frame.has_track_marks self#get_frame

    method frame_track_mark =
      match Frame.track_marks self#get_frame with
        | pos :: _ -> Some pos
        | _ -> None

    method frame_metadata = Frame.get_all_metadata self#get_frame
    method frame_position = Frame.position self#get_frame
    method frame_audio_position = Frame.audio_of_main self#frame_position

    (* If possible, end the current track.
       Typically, that signal is just re-routed, or makes the next file
       to be played if there's anything like a file. *)
    method virtual abort_track : unit
    val mutable buffer = None

    method buffer =
      match buffer with
        | Some buffer -> buffer
        | None ->
            let buf = Generator.create ~log:self#log self#content_type in
            buffer <- Some buf;
            buf

    val mutable empty_frame = None

    method empty_frame =
      match empty_frame with
        | Some frame -> frame
        | None ->
            let f = Frame.create ~length:0 self#content_type in
            empty_frame <- Some f;
            f

    val insert_metadata = Atomic.make None

    method insert_metadata ~new_track m =
      Atomic.set insert_metadata (Some (new_track, m))

    method end_of_track = Frame.add_track_mark self#empty_frame 0
    val mutable last_metadata = None
    method last_metadata = last_metadata
    val mutable on_frame : on_frame list = []
    method on_frame fn = on_frame <- on_frame @ [fn]
    val mutable reset_last_metadata_on_track = Atomic.make true

    method reset_last_metadata_on_track =
      Atomic.get reset_last_metadata_on_track

    method set_reset_last_metadata_on_track v =
      Atomic.set reset_last_metadata_on_track v

    method clear_last_metadata = last_metadata <- None

    method private set_last_metadata buf =
      match List.rev (Frame.get_all_metadata buf) with
        | v :: _ -> last_metadata <- Some v
        | _ -> ()

    val mutable on_track_called = false

    initializer
      self#on_before_streaming_cycle (fun () -> on_track_called <- false)

    method private execute_on_track buf =
      if not on_track_called then (
        on_track_called <- true;
        if self#reset_last_metadata_on_track then self#clear_last_metadata;
        self#set_last_metadata buf;
        let _, m =
          Option.value ~default:(0, Frame.Metadata.empty) last_metadata
        in
        self#log#debug "calling on_track handlers..";
        List.iter (function `Track fn -> fn m | _ -> ()) on_frame)

    val mutable last_images = Hashtbl.create 0

    method last_image field =
      match Hashtbl.find_opt last_images field with
        | Some i -> i
        | None ->
            let width, height = self#video_dimensions in
            let i = Video.Canvas.Image.create width height in
            Hashtbl.replace last_images field i;
            i

    method private set_last_image ~field img =
      Hashtbl.replace last_images field img

    val mutable video_generators = Hashtbl.create 0

    method private video_generator ~priv field =
      match Hashtbl.find_opt video_generators (priv, field) with
        | Some g -> g
        | None ->
            let params =
              Content.Video.get_params
                (Frame.Fields.find field self#content_type)
            in
            let g = Content.Video.make_generator params in
            Hashtbl.replace video_generators (priv, field) g;
            g

    method private internal_generate_video ?create ~priv ~field length =
      Content.Video.generate ?create (self#video_generator ~priv field) length

    method private generate_video = self#internal_generate_video ~priv:false

    method private nearest_image ~pos ~last_image buf =
      let nearest =
        List.fold_left
          (fun current (p, img) ->
            match current with
              | Some (p', _) when abs (p' - pos) < abs (p - pos) -> current
              | _ -> Some (p, img))
          None buf.Content.Video.data
      in
      match nearest with Some (_, img) -> img | None -> last_image

    method private normalize_video ~field content =
      let buf = Content.Video.get_data content in
      let data = buf.Content.Video.data in
      let last_image =
        match List.rev data with
          | (_, img) :: _ ->
              self#set_last_image ~field img;
              img
          | [] -> self#last_image field
      in
      Content.Video.lift_data
        (self#internal_generate_video ~field ~priv:true
           ~create:(fun ~pos ~width:_ ~height:_ () ->
             self#nearest_image ~pos ~last_image buf)
           (Content.length content))

    method private normalize_video_content =
      Frame.Fields.mapi (fun field content ->
          if
            Content.Video.is_data content
            && Frame.Fields.mem field self#content_type
          then self#normalize_video ~field content
          else content)

    method private on_position ~end_of_track buf =
      let length = Frame.position buf in
      elapsed <- elapsed + length;
      let rem = self#remaining in
      self#set_last_metadata buf;
      let _, m =
        Option.value ~default:(0, Frame.Metadata.empty) last_metadata
      in
      let is_allowed = function
        | { executed = true } -> false
        | { allow_partial = true } when end_of_track -> true
        | { mode = `Elapsed; position } -> position () <= elapsed
        | { mode = `Remaining; position } -> 0 <= rem && rem <= position ()
      in
      let position = function
        | { mode = `Elapsed } -> Frame.seconds_of_main elapsed
        | { mode = `Remaining } -> Frame.seconds_of_main rem
      in
      List.iter
        (function
          | `Position p when is_allowed p ->
              p.executed <- true;
              p.on_position ~pos:(position p) m
          | _ -> ())
        on_frame

    method private instrumented_generate_frame =
      let start_time = Unix.gettimeofday () in
      let on_frame = self#mutexify (fun () -> on_frame) () in
      List.iter
        (function `Frame { before = true; on_frame } -> on_frame () | _ -> ())
        on_frame;
      let buf = self#normalize_video_content self#generate_frame in
      List.iter
        (function
          | `Frame { before = false; on_frame } -> on_frame () | _ -> ())
        on_frame;
      let end_time = Unix.gettimeofday () in
      let length = Frame.position buf in
      let buf =
        match (Atomic.exchange insert_metadata None, Frame.track_marks buf) with
          | Some (new_track, m), _ ->
              let m =
                Frame.Metadata.append
                  (Option.value ~default:Frame.Metadata.empty
                     (Frame.get_metadata buf 0))
                  m
              in
              let buf = Frame.add_metadata buf 0 m in
              if new_track then Frame.(add_track_mark (drop_track_marks buf)) 0
              else buf
          | None, p :: _ :: _ ->
              self#log#important
                "Source created multiple tracks in a single frame! Sub-frame \
                 tracks are not supported and are merged into a single one..";
              Frame.add_track_mark (Frame.drop_track_marks buf) p
          | _ -> buf
      in
      let has_track_mark =
        match self#split_frame buf with
          | buf, Some new_track ->
              if 0 < elapsed then self#on_position ~end_of_track:true buf;
              elapsed <- 0;
              if self#reset_last_metadata_on_track then self#clear_last_metadata;
              List.iter
                (function `Position p -> p.executed <- false | _ -> ())
                on_frame;
              self#on_position ~end_of_track:false new_track;
              true
          | buf, None ->
              self#on_position ~end_of_track:false buf;
              false
      in

      (* Executing track mark and metadata callbacks is tricky.
         - We want to preserve the order with which they are registered.
         - We want to execute track marks even without metadata.

      First: execute both kind of callback using the metadata from the frame: *)
      let metadata = Frame.get_all_metadata buf in
      List.iter
        (fun (p, m) ->
          self#log#debug
            "generate_frame: got metadata at position %d: calling handlers..." p;
          let is_on_track =
            match last_metadata with
              | Some (p', _)
                when (not on_track_called) && has_track_mark && p = p' ->
                  self#log#debug "calling on_track handlers..";
                  on_track_called <- true;
                  true
              | _ -> false
          in
          List.iter
            (fun cb ->
              match cb with
                | `Metadata fn -> fn m
                | `Track fn when is_on_track -> fn m
                | _ -> ())
            on_frame)
        metadata;

      (* Then, if we have a track mark but no metadata, executed the on_track callbacks. *)
      if has_track_mark && not on_track_called then self#execute_on_track buf;
      self#iter_watchers (fun w ->
          w.generate_frame ~start_time ~end_time ~length ~has_track_mark
            ~metadata);
      buf
  end

(** Entry-point sources, which need to actively perform some task. *)
and virtual active_operator ?stack ?clock ~name sources =
  object (self)
    inherit operator ?stack ?clock ~name sources
    method! source_type : source_type = `Active (self :> active)

    (** Do whatever needed when the latency gets too big and is reset. *)
    method virtual reset : unit
  end

(** Shortcuts for defining sources with no children *)

and virtual source ?stack ?clock ~name () =
  object
    inherit operator ?stack ?clock ~name []
  end

class virtual active_source ?stack ?clock ~name () =
  object
    inherit active_operator ?stack ?clock ~name []
  end

(* Reselect type. This drives the choice of next source.
   In cases where the underlying source returns the same
   choice after calling [get_source], we need to refuse
   that source unless if can continue filling up the frame
   that is being worked on. This what [`After_position]
   captures. *)
type reselect = [ `Ok | `Force | `After_position of int ]

class virtual generate_from_multiple_sources ~merge ~track_sensitive () =
  object (self)
    method virtual get_source : reselect:reselect -> unit -> source option
    method virtual split_frame : Frame.t -> Frame.t * Frame.t option
    method virtual empty_frame : Frame.t
    method virtual private execute_on_track : Frame.t -> unit
    method virtual private set_last_metadata : Frame.t -> unit
    method virtual reset_last_metadata_on_track : bool
    method virtual clear_last_metadata : unit
    method virtual log : Log.t
    method virtual id : string
    val mutable ready_source = None

    method private can_generate_frame =
      match
        self#get_source
          ~reselect:(if track_sensitive () then `Ok else `Force)
          ()
      with
        | Some s when s#is_ready ->
            ready_source <- Some s;
            true
        | _ ->
            ready_source <- None;
            false

    val mutable current_source = None

    method private begin_track buf =
      if merge () then Frame.drop_track_marks buf
      else (
        let buf =
          if Frame.position buf > 0 then Frame.add_track_mark buf 0 else buf
        in
        self#execute_on_track buf;
        buf)

    method private can_reselect ~(reselect : reselect) (s : source) =
      s#is_ready
      &&
        match reselect with
        | `Ok -> true
        | `Force -> false
        | `After_position p -> p < Frame.position s#get_frame

    method private continue_frame s =
      s#get_partial_frame (fun frame ->
          match self#split_frame frame with
            | buf, Some next_track when Frame.position buf = 0 -> (
                if self#reset_last_metadata_on_track then
                  self#clear_last_metadata;
                match current_source with
                  | Some s' when s == s' -> self#empty_frame
                  | _ -> self#begin_track next_track)
            | buf, _ -> buf)

    method private generate_frame =
      let s = Option.get ready_source in
      assert s#is_ready;
      let buf = self#continue_frame s in
      let size = Lazy.force Frame.size in
      let rec f ~last_source ~last_chunk buf =
        let pos = Frame.position buf in
        let last_chunk_pos = Frame.position last_chunk in
        self#set_last_metadata last_chunk;
        if pos < size then (
          let rem = size - pos in
          match
            self#get_source ~reselect:(`After_position last_chunk_pos) ()
          with
            | Some s when last_source == s ->
                let remainder =
                  s#get_partial_frame (fun frame ->
                      if Frame.position frame <= last_chunk_pos then (
                        self#log#critical
                          "Source %s was re-selected but did not produce \
                           enough data: %d <! %d"
                          s#id (Frame.position frame) last_chunk_pos;
                        assert false);
                      Frame.slice frame (last_chunk_pos + rem))
                in
                let new_track = Frame.after remainder last_chunk_pos in
                let new_track =
                  if merge () then Frame.drop_track_marks new_track
                  else new_track
                in
                f ~last_source ~last_chunk:remainder
                  (Frame.append buf new_track)
            | Some s ->
                if not s#is_ready then (
                  self#log#critical "Underlying source %s is not ready!" s#id;
                  assert false);
                let new_track =
                  s#get_partial_frame (fun frame ->
                      match self#split_frame frame with
                        | buf, _ when Frame.position buf = 0 ->
                            Frame.slice frame rem
                        | buf, _ -> Frame.slice buf rem)
                in
                f ~last_source:s ~last_chunk:new_track
                  (Frame.append buf (self#begin_track new_track))
            | _ -> (last_source, buf))
        else (last_source, buf)
      in
      let last_source, buf = f ~last_source:s ~last_chunk:buf buf in
      current_source <- Some last_source;
      buf
  end
