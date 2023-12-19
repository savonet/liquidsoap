(*****************************************************************************

  Liquidsoap, a programmable stream generator.
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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

open Liquidsoap_lang.Error

(** In this module we define the central streaming concepts: sources, active
    sources and clocks.

    Sources can produce a stream, if something pulls it.
    Sources can pull streams from other sources (such non-elementary sources
    are called operators). But who starts pulling?

    Some sources have a noticeable effect, for example outputs.
    Some are indirectly needed by outputs.
    Some are useless, they have no direct or indirect observable effect.
    We only want to pull data from sources that have an effect,
    thereby "animating them". Those sources are called active.

    Clocks are in charge of animating active sources.
    Each clock "owns" a number of active sources, and indirectly some
    sources owned by those active sources, and controls access to their
    streams. *)

module Pcre = Re.Pcre

(** Fallibility type MUST be defined BEFORE clocks.
    Otherwise the module cannot be well-typed since the list of all
    clock variables refers to active sources and hence to #stype : source_t.
    Don't mess with this, type errors will give you a hard time!
    (By the way, there's no problem of scope escaping for class type
    since those are structural, not nominal like a variant type.) *)
type source_t = [ `Fallible | `Infallible ]

exception Unavailable

type streaming_state =
  [ `Unavailable | `Ready of unit -> unit | `Done of Frame.t ]

(** {1 Proto clocks}

    Roughly describe what a clock is, and build a notion of clock variable
    on top of that. More concrete clock stuff is done the [Clock] module.

    Clocks play two roles:
     (1) making sure that one source belongs to only one time flow,
     (2) giving a handle on how to run a time flow.

    Most clocks are passive, i.e. they don't run anything
    directly, but may only tick when something happens to a source.
    The clock is the default, active clock:
    when started, it launches a thread which keeps ticking regularly.

    A clock needs to know all the active sources under its control,
    so it can execute them. This might seem surprising in some cases:
      cross(s)       <-- create a clock, assigns it to s
      output.file(s) <-- also assigns it to the output
    In effect, we make it equivalent to
      cross(output.file(s))
    Anyway, it'd be very strange that an output isn't animated at all.

    Clock variables can represent an unknown clock, with attached outputs.
    A source gets assigned a clock variable, which might leave it
    a chance to choose that clock (by attempting to unify it).

    The idea is that when an output is created it assigns a clock to itself
    according to its sources' clocks. Eventually, all remaining unknown clocks
    are forced to clock.
 *)

type sync = [ `Auto | `CPU | `None ]
type self_sync = [ `Static | `Dynamic ] * bool

class type ['a, 'b] proto_clock =
  object
    method id : string
    method sync_mode : sync
    method start : bool
    method stop : unit

    (** Attach an active source, detach active sources by filter. *)

    method attach : 'a -> unit
    method detach : ('a -> bool) -> unit
    method is_attached : 'a -> bool

    (** Attach a sub_clock, get all subclocks, see below. *)

    method attach_clock : 'b -> unit
    method detach_clock : 'b -> unit
    method sub_clocks : 'b list
    method start_outputs : ('a -> bool) -> unit -> 'a list
    method on_before_output : (unit -> unit) -> unit
    method on_output : (unit -> unit) -> unit
    method on_after_output : (unit -> unit) -> unit
    method get_tick : int
    method end_tick : unit
  end

(** {1 Clock variables}
    Used to infer what clock a source belongs to.
    Each variable comes with
      - a list of active sources belonging to the clock, unused during
        inference/unification, but animated by the clock when running
      - a list of sub-clocks, used during unification's occurs-check
        to avoid cycles which would result in unsound behavior
        e.g. add([s,cross(f,s)]).
    Clock constants are objects of type [proto_clock], but need to also
    maintain the information attached to variables.

    The unification algorithm can be described as follows, ignoring
    the active source maintenance.
    X[Y1,Y2,..,Yn] denotes a variable or constant clock with the set Gamma
       of subclocks,
       from a first-order unification perspective it should be thought of
       as a term X(Y1,Y2,..,Yn,...) where the second ... denotes a
       row variable: we don't know if there are more parameters there
       (more subclocks)
    We write X[Gamma] with Gamma list of clocks, and X[..Y..] when
    Y belongs to the subclocks of X, or the subclocks of the subclocks,
    etc.
    Unification rules are:
      X[..Y..] = Y[..]    ---> ERROR (occurs-check)
      c1[...]  = c2[...]  ---> ERROR (rigid-rigid)
      X[Gamma] = Y[Delta] ---> X,Y:=Z[Gamma,Delta]
         Here Gamma,Delta denotes an union. It is possible that two
         distinct variables might become unified, in which case we'll
         end up with two occurrences of the same subclock.
  *)

(** Clock variables. *)
type 'a var =
  | Link of 'a link_t ref  (** a universal variable *)
  | Known of ('a, 'a var) proto_clock  (** a constant variable *)

and 'a unknown_source = {
  start : bool option;
  sources : 'a list;
  sub_clocks : 'a var list;
}

(** Contents of a clock variable. *)
and 'a link_t =
  | Unknown of 'a unknown_source
      (** the clock variable is unknown but depends on other variables *)
  | Same_as of 'a var  (** the clock variable is substituted by another *)

let debug = Sys.getenv_opt "LIQUIDSOAP_DEBUG" <> None
let create_known c = Known c

let create_unknown ?start ~sources ~sub_clocks () =
  Link (ref (Unknown { start; sources; sub_clocks }))

let rec deref = function Link { contents = Same_as a } -> deref a | x -> x

let rec variable_to_string = function
  | Link { contents = Same_as c } -> variable_to_string c
  | Link ({ contents = Unknown { start; sources; sub_clocks } } as r) ->
      Printf.sprintf "{id:%x,start:%s,sources=[%s],sub_clocks=[%s]}"
        (Obj.magic r)
        (match start with None -> "default" | Some v -> string_of_bool v)
        (String.concat "," (List.map (fun s -> s#id) sources))
        (String.concat "," (List.map variable_to_string sub_clocks))
  | Known c ->
      Printf.sprintf "%s[%s]" c#id
        (String.concat "," (List.map variable_to_string c#sub_clocks))

(** Equality modulo dereferencing, does not identify two variables with the same
    sources and clocks. *)
let var_eq a b =
  let a = deref a in
  let b = deref b in
  match (a, b) with
    | Link a, Link b -> a == b
    | Known a, Known b -> a = b
    | _, _ -> false

let rec sub_clocks = function
  | Known c -> c#sub_clocks
  | Link { contents = Unknown { sub_clocks } } -> sub_clocks
  | Link { contents = Same_as x } -> sub_clocks x

let occurs_check ~pos x y =
  let rec aux = function
    | [] -> ()
    | [] :: tl -> aux tl
    | (x' :: clocks) :: tl ->
        if var_eq x x' then
          raise (Clock_loop (pos, variable_to_string x, variable_to_string y));
        aux (sub_clocks x' :: clocks :: tl)
  in
  aux [sub_clocks y]

let occurs_check ~pos x y =
  occurs_check ~pos x y;
  occurs_check ~pos y x

let rec unify ~pos a b =
  match (a, b) with
    | Link { contents = Same_as a }, _ -> unify ~pos a b
    | _, Link { contents = Same_as b } -> unify ~pos a b
    | Known s, Known s' ->
        if s <> s' then
          raise
            (Clock_conflict (pos, variable_to_string a, variable_to_string b))
    | Link ra, Link rb when ra == rb -> ()
    | ( Link
          ({ contents = Unknown { start; sources = sa; sub_clocks = ca } } as
          ra),
        Link
          ({
             contents =
               Unknown { start = start'; sources = sb; sub_clocks = cb };
           } as rb) ) ->
        let start =
          match (start, start') with
            | None, s | s, None -> s
            | Some v, Some v' when v = v' -> Some v
            | _ ->
                raise
                  (Clock_conflict
                     (pos, variable_to_string a, variable_to_string b))
        in
        occurs_check ~pos a b;
        let merge =
          let s = List.sort_uniq Stdlib.compare (List.rev_append sa sb) in
          let sc = List.sort_uniq Stdlib.compare (List.rev_append ca cb) in
          Link (ref (Unknown { start; sources = s; sub_clocks = sc }))
        in
        ra := Same_as merge;
        rb := Same_as merge
    | ( Known c,
        Link
          ({ contents = Unknown { start; sources = s; sub_clocks = sc } } as r)
      )
    | ( Link
          ({ contents = Unknown { start; sources = s; sub_clocks = sc } } as r),
        Known c ) ->
        if start <> None && c#start <> Option.get start then
          raise
            (Clock_conflict (pos, variable_to_string a, variable_to_string b));
        occurs_check ~pos (Known c) (Link r);
        List.iter c#attach s;
        List.iter c#attach_clock sc;
        r := Same_as (Known c)

let rec forget var subclock =
  match var with
    | Known c -> c#detach_clock subclock
    | Link { contents = Same_as a } -> forget a subclock
    | Link ({ contents = Unknown ({ sub_clocks } as c) } as r) ->
        r :=
          Unknown
            { c with sub_clocks = List.filter (( <> ) subclock) sub_clocks }

(** {1 Sources} *)

(** Instrumentation. *)

type metadata = (int * Frame.metadata) list
type clock_sync_mode = [ sync | `Unknown ]

type watcher = {
  wake_up :
    stype:source_t ->
    is_active:bool ->
    id:string ->
    ctype:Frame.content_type ->
    clock_id:string ->
    clock_sync_mode:clock_sync_mode ->
    unit;
  sleep : unit -> unit;
  generate_frame :
    start_time:float ->
    end_time:float ->
    length:int ->
    has_track_mark:bool ->
    metadata:metadata ->
    unit;
  before_output : unit -> unit;
  after_output : unit -> unit;
}

let source_log = Log.make ["source"]

(** Has any output been created? This is used by Main to decide if there's
    anything "to run". Note that we could get rid of it, since outputs (active
    sources) are actually registered to clock variables. *)
let has_outputs = ref false

let add_new_output, iterate_new_outputs =
  let lock = Mutex.create () in
  let l = ref [] in
  ( Tutils.mutexify lock (fun x -> l := x :: !l),
    Tutils.mutexify lock (fun f ->
        List.iter f !l;
        l := []) )

class virtual operator ?pos ?(name = "src") sources =
  let frame_type = Type.var () in
  object (self)
    (** Monitoring *)
    val mutable watchers = []

    method add_watcher w = watchers <- w :: watchers
    method private iter_watchers fn = List.iter fn watchers
    val mutable pos : Pos.Option.t = pos
    method pos = pos
    method set_pos p = pos <- p

    (** Logging and identification *)

    val mutable log = source_log
    method private create_log = log <- Log.make [self#id]
    method log = log
    val mutable id = ""
    val mutable definitive_id = false
    val mutable name = name
    method set_name n = name <- n
    initializer id <- Lang_string.generate_id name
    method id = id

    method set_id ?(definitive = true) s =
      let s =
        Pcre.substitute ~rex:(Pcre.regexp "[ \t\n.]") ~subst:(fun _ -> "_") s
      in
      if not definitive_id then (
        id <- Lang_string.generate_id s;
        definitive_id <- definitive);

      (* Sometimes the ID is changed during initialization, in order to make it
         equal to the server name, which is only registered at initialization
         time in order to avoid bloating from unused sources. If the ID
         changes, and [log] has already been initialized, reset it. *)
      if log != source_log then self#create_log

    initializer
      if debug then
        Gc.finalise (fun s -> source_log#info "Garbage collected %s." s#id) self

    val mutex = Mutex.create ()

    method private mutexify : 'a 'b. ('a -> 'b) -> 'a -> 'b =
      Tutils.mutexify mutex

    (** Is the source infallible, i.e. is it always guaranteed that there will
        be always be a next track immediately available. *)
    method virtual stype : source_t

    (** Is the source active *)
    method is_active = false

    (** Children sources *)
    val mutable sources : operator list = sources

    (* Clock setup
       Each source starts with an unknown clock.
       This clock will be unified with children clocks in most cases.
       Once the clock has been set to a concrete clock, it cannot be
       changed anymore: a source lives in only one time flow.

       We need a #set_clock method with a default behavior that can
       be overridden, and it needs to be called at initialization:
       #wake_up is too late since it's the clock who initiates it. *)
    val clock : active_operator var =
      create_unknown ~sources:[] ~sub_clocks:[] ()

    method clock = clock
    method virtual self_sync : self_sync

    method private set_clock =
      List.iter (fun s -> unify ~pos self#clock s#clock) sources

    initializer self#set_clock

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
                    source %s!"
                   self#id);
            self#log#debug "Assigning source content type for frame type: %s"
              (Type.to_string self#frame_type);
            let ct = Frame_type.content_type self#frame_type in
            self#log#debug "Content type: %s" (Frame.string_of_content_type ct);
            ctype <- Some ct;
            ct

    method private audio_channels =
      match Frame.Fields.find_opt Frame.Fields.audio self#content_type with
        | Some c when Content.Audio.is_format c ->
            Content.Audio.channels_of_format c
        | Some c when Content_pcm_s16.is_format c ->
            Content_pcm_s16.channels_of_format c
        | Some c when Content_pcm_f32.is_format c ->
            Content_pcm_f32.channels_of_format c
        | _ -> raise Content.Invalid

    method private video_dimensions =
      Content.Video.dimensions_of_format
        (Option.get
           (Frame.Fields.find_opt Frame.Fields.video self#content_type))

    (** Startup/shutdown.

      Get the source ready for streaming on demand, have it release resources
      when it's not used any more, and decide whether the source should run in
      caching mode.

      A source may be accessed by several sources, and must switch to caching
      mode when it may be accessed by more than one source, in order to ensure
      consistency of the delivered stream chunk.

      Before that a source P accesses another source S it must activate it. The
      An activation is identified by the path to the source which required it.
      It is possible that two identical activations are done, and they should
      not be treated as a single one.

      It is assumed that all streaming is done in one thread for a given clock,
      so the activation management API is not thread-safe. *)

    val mutable caching = false
    val mutable activations : operator list list = []
    val mutable on_wake_up = []
    method on_wake_up fn = on_wake_up <- fn :: on_wake_up

    initializer
      self#on_wake_up (fun () ->
          let clock_id, clock_sync_mode =
            match deref self#clock with
              | Known c -> (c#id, (c#sync_mode :> clock_sync_mode))
              | _ -> ("unknown", `Unknown)
          in
          self#iter_watchers (fun w ->
              w.wake_up ~stype:self#stype ~is_active:self#is_active ~id:self#id
                ~ctype:self#content_type ~clock_id ~clock_sync_mode))

    (* Ask for initialization.
       The current implementation makes it dangerous to call #get_ready from
       another thread than the Root one, as interleaving with #get is
       forbidden. *)
    method get_ready (activation : operator list) =
      self#content_type_computation_allowed;
      if log == source_log then self#create_log;
      if activations = [] then (
        source_log#info "Source %s gets up with content type: %s." id
          (Frame.string_of_content_type self#content_type);
        self#wake_up activation;
        List.iter (fun fn -> fn ()) on_wake_up);
      activations <- activation :: activations

    val mutable on_sleep = []
    method on_sleep fn = on_sleep <- fn :: on_sleep

    initializer
      self#on_sleep (fun () -> self#iter_watchers (fun w -> w.sleep ()))

    (* Release the source, which will shutdown if possible.
       The current implementation makes it dangerous to call #leave from
       another thread than the Root one, as interleaving with #get is
       forbidden. *)
    method leave ?(failed_to_start = true) src =
      let rec remove acc = function
        | [] when failed_to_start -> []
        | [] ->
            self#log#critical "Got ill-balanced activations (from %s)!" src#id;
            assert false
        | (s :: _) :: tl when s = src -> List.rev_append acc tl
        | h :: tl -> remove (h :: acc) tl
      in
      activations <- remove [] activations;
      if activations = [] then (
        source_log#info "Source %s gets down." id;
        self#sleep;
        List.iter (fun fn -> try fn () with _ -> ()) on_sleep)

    method is_up = activations <> []

    (** Two methods called for initialization and shutdown of the source *)
    method private wake_up activation =
      self#log#debug "Clock is %s." (variable_to_string self#clock);
      self#log#important "Content type is %s."
        (Frame.string_of_content_type self#content_type);
      let activation = (self :> operator) :: activation in
      List.iter (fun s -> s#get_ready activation) sources

    method private sleep =
      List.iter
        (fun s -> s#leave ?failed_to_start:None (self :> operator))
        sources

    (** Streaming *)

    (* Number of frames left in the current track: -1 means Infinity, time unit
       is the frame. *)
    method virtual remaining : int
    val mutable elapsed = 0
    method elapsed = elapsed

    method duration =
      let r = self#remaining in
      let e = self#elapsed in
      if r < 0 || e < 0 then -1 else e + r

    method virtual seek_source : source

    method seek n =
      let s = self#seek_source in
      if (s :> < seek : int -> int >) == (self :> < seek : int -> int >) then 0
      else s#seek n

    method virtual private can_generate_frame : bool
    method virtual private generate_frame : Frame.t
    val mutable streaming_state : streaming_state = `Unavailable
    method streaming_state = streaming_state

    method is_ready =
      self#has_ticked;
      match streaming_state with `Ready _ | `Done _ -> true | _ -> false

    val mutable _cache = None

    method private cache =
      match _cache with None -> self#empty_frame | Some c -> c

    val mutable consumed = 0

    (* This is the implementation of the main streaming logic. *)
    initializer
      self#on_before_output (fun () ->
          consumed <- 0;
          let cache = self#cache in
          let cache_pos = Frame.position cache in
          let size = Lazy.force Frame.size in
          if cache_pos > 0 || self#can_generate_frame then
            streaming_state <-
              `Ready
                (fun () ->
                  let buf =
                    if cache_pos < size then
                      Frame.append cache self#instrumented_generate_frame
                    else cache
                  in
                  let buf_pos = Frame.position buf in
                  let buf =
                    if size < buf_pos then (
                      _cache <-
                        Some
                          (Frame.chunk ~start:size ~stop:(buf_pos - size) buf);
                      Frame.slice buf size)
                    else buf
                  in
                  streaming_state <- `Done buf)
          else streaming_state <- `Unavailable);

      self#on_after_output (fun () ->
          match (streaming_state, consumed) with
            | `Done _, 0 -> _cache <- None
            | `Done buf, n ->
                let pos = Frame.position buf in
                assert (n <= pos);
                if pos = n then _cache <- None
                else _cache <- Some (Frame.chunk ~start:n ~stop:(pos - n) buf)
            | _ -> ())

    method get_partial_frame cb =
      self#has_ticked;
      match streaming_state with
        | `Unavailable ->
            log#critical "source called while not ready!";
            raise Unavailable
        | `Ready fn ->
            fn ();
            self#get_partial_frame cb
        | `Done data ->
            let data = cb data in
            consumed <- max consumed (Frame.position data);
            data

    method get_frame = self#get_partial_frame (fun f -> f)

    method get_mutable_content field =
      let content = Frame.get self#get_frame field in
      (* Optimization is disabled for now. *)
      Content.copy content

    method get_mutable_frame field =
      Frame.set self#get_frame field (self#get_mutable_content field)

    method set_data
        : 'a.
          Frame.field ->
          (?offset:int -> ?length:int -> 'a -> Content.data) ->
          'a ->
          Frame.t =
      fun field lift data -> Frame.set_data self#get_frame field lift data

    method private split_frame frame =
      match Frame.track_marks frame with
        | 0 :: _ -> (self#empty_frame, Some frame)
        | p :: _ ->
            ( Frame.slice frame p,
              Some (Frame.chunk ~start:p ~stop:(Frame.position frame) frame) )
        | [] -> (frame, None)

    method has_track_mark = Frame.has_track_marks self#get_frame

    method track_mark =
      match Frame.track_marks self#get_frame with
        | pos :: _ -> Some pos
        | _ -> None

    method metadata = Frame.get_all_metadata self#get_frame
    method position = Frame.position self#get_frame
    method audio_position = Frame.audio_of_main self#position

    (* This is rounded up. *)
    method video_position =
      Frame.video_of_main (self#position + Lazy.force Frame.video_rate - 1)

    (* If possible, end the current track.
       Typically, that signal is just re-routed, or makes the next file
       to be played if there's anything like a file. *)
    method virtual abort_track : unit
    val mutable buffer = None

    method buffer =
      match buffer with
        | Some buffer -> buffer
        | None ->
            let buf =
              Generator.create ~log:(self#log#info "%s") self#content_type
            in
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

    method end_of_track = Frame.add_track_mark self#empty_frame 0
    val mutable last_metadata = None
    method last_metadata = last_metadata
    val mutable on_metadata : (Frame.metadata -> unit) List.t = []
    method on_metadata fn = on_metadata <- fn :: on_metadata
    val mutable on_track_called = false
    initializer self#on_before_output (fun () -> on_track_called <- false)
    val mutable on_track : (Frame.metadata -> unit) List.t = []
    method on_track fn = on_track <- fn :: on_track

    method private execute_on_track buf =
      if not on_track_called then (
        on_track_called <- true;
        let m =
          match List.rev (Frame.get_all_metadata buf) with
            | (_, m) :: _ -> m
            | [] -> Frame.Metadata.empty
        in
        List.iter (fun fn -> fn m) on_track)

    method private instrumented_generate_frame =
      let start_time = Unix.gettimeofday () in
      let buf = self#generate_frame in
      let end_time = Unix.gettimeofday () in
      let length = Frame.position buf in
      let track_marks = Frame.track_marks buf in
      let buf =
        match track_marks with
          | p :: _ :: _ ->
              self#log#important
                "Source created multiple tracks in a single frame! Sub-frame \
                 tracks are not supported and are merged into a single one..";
              Frame.add_track_mark (Frame.drop_track_marks buf) p
          | _ -> buf
      in
      let has_track_mark = track_marks <> [] in
      if has_track_mark then elapsed <- 0 else elapsed <- elapsed + length;
      let metadata = Frame.get_all_metadata buf in
      let on_metadata = self#mutexify (fun () -> on_metadata) () in
      List.iter
        (fun (i, m) ->
          self#log#debug "Got metadata at position %d: calling handlers..." i;
          List.iter (fun fn -> fn m) on_metadata)
        metadata;
      (match List.rev (Frame.get_all_metadata buf) with
        | (_, m) :: _ -> last_metadata <- Some m
        | [] -> ());
      if has_track_mark then self#execute_on_track buf;
      self#iter_watchers (fun w ->
          w.generate_frame ~start_time ~end_time ~length ~has_track_mark
            ~metadata);
      buf

    (* Set to [true] when we're inside an output cycle. *)
    val mutable in_output = false
    val mutable on_before_output = []
    method on_before_output fn = on_before_output <- fn :: on_before_output

    initializer
      self#on_before_output (fun () ->
          self#iter_watchers (fun w -> w.before_output ()))

    (* Prepare for output round. *)
    method private before_output =
      List.iter (fun fn -> fn ()) on_before_output;
      in_output <- true

    val mutable on_output : (unit -> unit) List.t = []
    method on_output fn = on_output <- fn :: on_output
    val mutable on_after_output = []
    method on_after_output fn = on_after_output <- fn :: on_after_output

    initializer
      self#on_after_output (fun () ->
          self#iter_watchers (fun w -> w.after_output ()))

    (* Cleanup after output round. *)
    method private after_output =
      List.iter (fun fn -> fn ()) on_after_output;
      in_output <- false

    method private has_ticked =
      match deref clock with
        | Known c ->
            if not in_output then (
              in_output <- true;
              self#before_output;
              c#on_output (fun () -> List.iter (fun fn -> fn ()) on_output);
              c#on_after_output (fun () -> self#after_output))
        | _ -> ()
  end

(** Entry-point sources, which need to actively perform some task. *)
and virtual active_operator ?pos ?name sources =
  object (self)
    inherit operator ?pos ?name sources

    initializer
      has_outputs := true;
      add_new_output (self :> active_operator);
      ignore
        (unify ~pos:self#pos self#clock
           (create_unknown
              ~sources:[(self :> active_operator)]
              ~sub_clocks:[] ()))

    method! is_active = true

    (** Start a new output round, may trigger the computation of a frame. *)
    method virtual output : unit

    (** Do whatever needed when the latency gets too big and is reset. *)
    method virtual reset : unit
  end

(** Shortcuts for defining sources with no children *)

and virtual source ?pos ?name () =
  object
    inherit operator ?pos ?name []
  end

class virtual active_source ?pos ?name () =
  object
    inherit active_operator ?pos ?name []
  end

class virtual generate_from_multiple_sources ~merge ~track_sensitive () =
  object (self)
    method virtual get_source : reselect:bool -> unit -> source option
    method virtual split_frame : Frame.t -> Frame.t * Frame.t option
    method virtual empty_frame : Frame.t
    method virtual private execute_on_track : Frame.t -> unit

    method private can_generate_frame =
      match self#get_source ~reselect:(not (track_sensitive ())) () with
        | Some s -> s#is_ready
        | None -> false

    val mutable current_source = None

    method private continue_frame s =
      s#get_partial_frame (fun frame ->
          match self#split_frame frame with
            | buf, Some next_track when Frame.position buf = 0 ->
                if match current_source with Some s' -> s != s' | None -> true
                then
                  if merge () then Frame.drop_track_marks next_track
                  else (
                    self#execute_on_track next_track;
                    Frame.add_track_mark next_track 0)
                else self#empty_frame
            | buf, _ -> buf)

    method private generate_frame =
      let s = Option.get (self#get_source ~reselect:false ()) in
      assert s#is_ready;
      let buf = self#continue_frame s in
      let size = Lazy.force Frame.size in
      let rec f ~last_source ~last_buf buf =
        let pos = Frame.position buf in
        if pos < size then (
          match self#get_source ~reselect:true () with
            | Some s' when last_source == s' ->
                let remainder =
                  s#get_partial_frame (fun frame ->
                      Frame.slice frame (Frame.position last_buf + size - pos))
                in
                ( s,
                  Frame.append buf
                    (Frame.chunk ~start:(Frame.position last_buf)
                       ~stop:(Frame.position remainder - Frame.position last_buf)
                       remainder) )
            | Some s ->
                assert s#is_ready;
                let new_track =
                  s#get_partial_frame (fun frame ->
                      match self#split_frame frame with
                        | buf, _ when Frame.position buf = 0 ->
                            Frame.slice frame (size - pos)
                        | buf, _ ->
                            Frame.slice frame
                              (min (Frame.position buf) size - pos))
                in
                let new_track =
                  if merge () then Frame.drop_track_marks new_track
                  else (
                    self#execute_on_track new_track;
                    Frame.add_track_mark new_track 0)
                in
                f ~last_source:s ~last_buf:new_track
                  (Frame.append buf new_track)
            | _ -> (last_source, buf))
        else (last_source, buf)
      in
      let last_source, buf = f ~last_source:s ~last_buf:self#empty_frame buf in
      current_source <- Some last_source;
      buf
  end

(** Specialized shortcuts *)

type clock_variable = active_source var

class type clock =
  object
    method id : string
    method start : bool
    method stop : unit
    method sync_mode : sync
    method attach : active_source -> unit
    method detach : (active_source -> bool) -> unit
    method is_attached : active_source -> bool
    method attach_clock : clock_variable -> unit
    method detach_clock : clock_variable -> unit
    method sub_clocks : clock_variable list
    method start_outputs : (active_source -> bool) -> unit -> active_source list
    method on_before_output : (unit -> unit) -> unit
    method on_output : (unit -> unit) -> unit
    method on_after_output : (unit -> unit) -> unit
    method get_tick : int
    method end_tick : unit
  end

module Clock_variables = struct
  let to_string = variable_to_string
  let create_unknown = create_unknown
  let create_known = create_known

  let subclocks v =
    match deref v with
      | Link { contents = Unknown { sub_clocks } } -> sub_clocks
      | _ -> assert false

  let unify = unify
  let forget = forget
  let get v = match deref v with Known c -> c | _ -> assert false
  let is_known v = match deref v with Known _ -> true | _ -> false

  let should_start v =
    match deref v with
      | Link { contents = Unknown { start } } ->
          Option.value ~default:true start
      | _ -> assert false
end

let has_outputs () = !has_outputs
