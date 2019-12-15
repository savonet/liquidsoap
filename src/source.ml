(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

(** In this module we define the central streaming concepts: sources, active
  * sources and clocks.
  *
  * Sources can produce a stream, if something pulls it.
  * Sources can pull streams from other sources (such non-elementary sources
  * are called operators).
  * But who starts pulling?
  *
  * Some sources have a noticeable effect, for example outputs.
  * Some are indirectly needed by outputs.
  * Some are useless, they have no direct or indirect observable effect.
  * We only want to pull data from sources that have an effect,
  * thereby "animating them". Those sources are called active.
  *
  * Clocks are in charge of animating active sources.
  * Each clock "owns" a number of active sources, and indirectly some
  * sources owned by those active sources,
  * and controls access to their streams. *)

(** Fallibility type MUST be defined BEFORE clocks.
  * Otherwise the module cannot be well-typed since the list of all
  * clock variables refers to active sources and hence to #stype : source_t.
  * Don't mess with this, type errors will give you a hard time!
  * (By the way, there's no problem of scope escaping for class type
  *  since those are structural, not nominal like a variant type.) *)
type source_t = Fallible | Infallible

(** {1 Proto clocks} 
  *
  * Roughly describe what a clock is, and build a notion of clock variable
  * on top of that. More concrete clock stuff is done the [Clock] module.
  *
  * Clocks play two roles:
  *  (1) making sure that one source belongs to only one time flow,
  *  (2) giving a handle on how to run a time flow.
  *
  * Most clocks are passive, i.e. they don't run anything
  * directly, but may only tick when something happens to a source.
  * The clock is the default, active clock:
  * when started, it launches a thread which keeps ticking regularly.
  *
  * A clock needs to know all the active sources under its control,
  * so it can execute them. This might seem surprising in some cases:
  *   cross(s)       <-- create a clock, assigns it to s
  *   output.file(s) <-- also assigns it to the output
  * In effect, we make it equivalent to
  *   cross(output.file(s))
  * Anyway, it'd be very strange that an output isn't animated at all.
  *
  * Clock variables can represent an unknown clock, with attached outputs.
  * A source gets assigned a clock variable, which might leave it
  * a chance to choose that clock (by attempting to unify it).
  *
  * The idea is that when an output is created it assigns a clock to itself
  * according to its sources' clocks. Eventually, all remaining unknown clocks
  * are forced to clock. *)

type sync = [ `Auto | `CPU | `None ]

class type ['a, 'b] proto_clock =
  object
    method id : string

    method sync_mode : sync

    (** Attach an active source, detach active sources by filter. *)

    method attach : 'a -> unit

    method detach : ('a -> bool) -> unit

    (** Attach a sub_clock, get all subclocks, see below. *)

    method attach_clock : 'b -> unit

    method detach_clock : 'b -> unit

    method sub_clocks : 'b list

    method start_outputs : ('a -> bool) -> unit -> 'a list

    method get_tick : int

    method end_tick : unit
  end

(** {1 Clock variables}
  * Used to infer what clock a source belongs to.
  * Each variable comes with
  *   - a list of active sources belonging to the clock, unused during
  *     inference/unification, but animated by the clock when running
  *   - a list of sub-clocks, used during unification's occurs-check
  *     to avoid cycles which would result in unsound behavior
  *     e.g. add([s,cross(f,s)]).
  * Clock constants are objects of type [proto_clock], but need to also
  * maintain the information attached to variables.
  *
  * The unification algorithm can be described as follows, ignoring
  * the active source maintenance.
  * X[Y1,Y2,..,Yn] denotes a variable or constant clock with the set Gamma
  *    of subclocks,
  *    from a first-order unification perspective it should be thought of
  *    as a term X(Y1,Y2,..,Yn,...) where the second ... denotes a
  *    row variable: we don't know if there are more parameters there
  *    (more subclocks)
  * We write X[Gamma] with Gamma list of clocks, and X[..Y..] when
  * Y belongs to the subclocks of X, or the subclocks of the subclocks,
  * etc.
  * Unification rules are:
  *   X[..Y..] = Y[..]    ---> ERROR (occurs-check)
  *   c1[...]  = c2[...]  ---> ERROR (rigid-rigid)
  *   X[Gamma] = Y[Delta] ---> X,Y:=Z[Gamma,Delta]
  *      Here Gamma,Delta denotes an union. It is possible that two
  *      distinct variables might become unified, in which case we'll
  *      end up with two occurrences of the same subclock.
  *)

(** Clock variables. *)
type 'a var =
  | Link of 'a link_t ref  (** a universal variable *)
  | Known of ('a, 'a var) proto_clock  (** a constant variable *)

(** Contents of a clock variable. *)
and 'a link_t =
  | Unknown of 'a list * 'a var list
      (** the clock variable is unknown but depends on other variables *)
  | Same_as of 'a var  (** the clock variable is subtituted by another *)

let debug = Utils.getenv_opt "LIQUIDSOAP_DEBUG" <> None
let create_known c = Known c

let create_unknown ~sources ~sub_clocks =
  Link (ref (Unknown (sources, sub_clocks)))

let rec deref = function Link { contents = Same_as a } -> deref a | x -> x

let rec variable_to_string = function
  | Link { contents = Same_as c } -> variable_to_string c
  | Link ({ contents = Unknown (sources, clocks) } as r) ->
      Printf.sprintf "?(%x:%d)[%s]" (Obj.magic r) (List.length sources)
        (String.concat "," (List.map variable_to_string clocks))
  | Known c ->
      Printf.sprintf "%s[%s]" c#id
        (String.concat "," (List.map variable_to_string c#sub_clocks))

(** Equality modulo dereferencing, does not identify two variables
  * with the same sources and clocks. *)
let var_eq a b =
  let a = deref a in
  let b = deref b in
  match (a, b) with
    | Link a, Link b -> a == b
    | Known a, Known b -> a = b
    | _, _ -> false

exception Clock_conflict of string * string
exception Clock_loop of string * string

let rec sub_clocks = function
  | Known c -> c#sub_clocks
  | Link { contents = Unknown (_, sc) } -> sc
  | Link { contents = Same_as x } -> sub_clocks x

let occurs_check x y =
  let rec aux = function
    | [] -> ()
    | [] :: tl -> aux tl
    | (x' :: clocks) :: tl ->
        if var_eq x x' then
          raise (Clock_loop (variable_to_string x, variable_to_string y));
        aux (sub_clocks x' :: clocks :: tl)
  in
  aux [sub_clocks y]

let occurs_check x y =
  occurs_check x y;
  occurs_check y x

let rec unify a b =
  match (a, b) with
    | Link { contents = Same_as a }, _ -> unify a b
    | _, Link { contents = Same_as b } -> unify a b
    | Known s, Known s' ->
        if s <> s' then
          raise (Clock_conflict (variable_to_string a, variable_to_string b))
    | ( Link ({ contents = Unknown (sa, ca) } as ra),
        Link ({ contents = Unknown (sb, cb) } as rb) ) ->
        (* TODO perhaps optimize ca@cb *)
        occurs_check a b;
        let merge = Link (ref (Unknown (sa @ sb, ca @ cb))) in
        ra := Same_as merge;
        rb := Same_as merge
    | Known c, Link ({ contents = Unknown (s, sc) } as r)
    | Link ({ contents = Unknown (s, sc) } as r), Known c ->
        occurs_check (Known c) (Link r);
        List.iter c#attach s;
        List.iter c#attach_clock sc;
        r := Same_as (Known c)

let rec forget var subclock =
  match var with
    | Known c -> c#detach_clock subclock
    | Link { contents = Same_as a } -> forget a subclock
    | Link ({ contents = Unknown (sources, clocks) } as r) ->
        r := Unknown (sources, List.filter (( <> ) subclock) clocks)

(** {1 Sources} *)

(** Instrumentation. *)

type metadata = (int * (string, string) Hashtbl.t) list
type clock_sync_mode = [ sync | `Unknown ]

type watcher = {
  get_ready :
    stype:source_t ->
    is_output:bool ->
    id:string ->
    content_kind:Frame.content_kind ->
    clock_id:string ->
    clock_sync_mode:clock_sync_mode ->
    unit;
  leave : unit -> unit;
  get_frame :
    start_time:float ->
    end_time:float ->
    start_position:int ->
    end_position:int ->
    is_partial:bool ->
    metadata:metadata ->
    unit;
  after_output : unit -> unit;
}

let source_log = Log.make ["source"]

(** Has any output been created? This is used by Main to decide if
  * there's anything "to run". Note that we could get rid of it, since
  * outputs (active sources) are actually registered to clock variables. *)
let has_outputs = ref false

let add_new_output, iterate_new_outputs =
  let lock = Mutex.create () in
  let l = ref [] in
  ( Tutils.mutexify lock (fun x -> l := x :: !l),
    Tutils.mutexify lock (fun f ->
        List.iter f !l;
        l := []) )

class virtual operator ?(name = "src") content_kind sources =
  object (self)
    (** Monitoring *)
    val mutable watchers = []

    method add_watcher w = watchers <- w :: watchers

    method private iter_watchers fn = List.iter fn watchers

    (** Logging and identification *)

    val mutable log = source_log

    method private create_log = log <- Log.make [self#id]

    method private log = log

    val mutable id = ""

    val mutable definitive_id = false

    initializer id <- name ^ "_" ^ string_of_int (Oo.id self)

    method id = id

    method set_id ?(definitive = true) s =
      (* ID musn't contain "." *)
      let s = Pcre.substitute ~pat:"\\." ~subst:(fun _ -> "(dot)") s in
      let s = Pcre.substitute ~pat:"[ \t\n]" ~subst:(fun _ -> "_") s in
      if not definitive_id then (
        id <- s;
        definitive_id <- definitive );
      (* Sometimes the ID is changed during initialization,
       * in order to make it equal to the server name,
       * which is only registered at initialization time in order
       * to avoid bloating from unused sources.
       * If the ID changes, and [log] has already been initialized, reset it. *)
      if log != source_log then self#create_log

    initializer
    if debug then
      Gc.finalise (fun s -> source_log#info "Garbage collected %s." s#id) self

    (** Is the source infallible, i.e. is it always guaranteed that there
    * will be always be a next track immediately available. *)
    method virtual stype : source_t

    (** Is the source active *)
    method is_output = false

    (** Children sources *)
    val mutable sources : operator list = sources

    (* Clock setup
     * Each source starts with an unknown clock.
     * This clock will be unified with children clocks in most cases.
     * Once the clock has been set to a concrete clock, it cannot be
     * changed anymore: a source lives in only one time flow.
     *
     * We need a #set_clock method with a default behavior that can
     * be overridden, and it needs to be called at initialization:
     * #wake_up is too late since it's the clock who initiates it. *)
    val clock : active_operator var = create_unknown ~sources:[] ~sub_clocks:[]

    method clock = clock

    method virtual self_sync : bool

    method private set_clock =
      List.iter (fun s -> unify self#clock s#clock) sources

    initializer self#set_clock

    (** Startup/shutdown.
    *
    * Get the source ready for streaming on demand, have it release resources
    * when it's not used any more, and decide whether the source should run in
    * caching mode.
    *
    * A source may be accessed by several sources, and must switch to caching
    * mode when it may be accessed by more than one source, in order to ensure
    * consistency of the delivered stream chunk.
    *
    * Before that a source P accesses another source S it must activate it. The
    * activation can be static, or dynamic. A static activation means that
    * P may pull data from S at any time. A dynamic activation means that P
    * won't use S directly but may at some point build a source which will
    * access S. This dynamic creation may occur in the middle of an output
    * round, which is why S needs to know in advance, since in some cases it
    * might have to enter caching mode from the beginning of the round in case
    * the dynamic activation occurs.
    *
    * An activation is identified by the path to the source which required it.
    * It is possible that two identical activations are done, and they should
    * not be treated as a single one.
    *
    * In short, a source can avoid caching when: it has only one static
    * activation and all its dynamic activations are sub-paths of the static
    * one. When there is no static activation, there cannot be any access.
    *
    * It is assumed that all streaming is done in one thread for a given clock,
    * so the activation management API is not thread-safe. *)

    val mutable caching = false

    val mutable dynamic_activations : operator list list = []

    val mutable static_activations : operator list list = []

    (* List of callbacks executed when source shuts down. *)
    val mutable on_shutdown = []

    val on_shutdown_m = Mutex.create ()

    method on_shutdown fn =
      (Tutils.mutexify on_shutdown_m (fun () ->
           on_shutdown <- fn :: on_shutdown))
        ()

    (* contains: (ns,descr,usage,name,f) *)
    val mutable commands = []

    val mutable ns_kind = "unknown"

    val mutable ns = []

    method register_command ~descr ?usage name f =
      commands <- (descr, usage, name, f) :: commands

    method private update_caching_mode =
      let string_of activations =
        String.concat ", "
          (List.map
             (fun l -> String.concat ":" (List.map (fun s -> s#id) l))
             activations)
      in
      self#log#info "Activations changed: static=[%s], dynamic=[%s]."
        (string_of static_activations)
        (string_of dynamic_activations);
      (* Decide whether caching mode is needed, and why *)
      match
        if self#is_output then Some "active source"
        else (
          match static_activations with
            | [] -> None
            | [s] ->
                if
                  List.exists
                    (fun d -> not (Utils.prefix (List.rev d) (List.rev s)))
                    dynamic_activations
                then Some "possible dynamic activation"
                else None
            | _ -> Some "two static activations" )
      with
        | None ->
            if caching then begin
              caching <- false;
              self#log#info "Disabling caching mode."
            end
        | Some msg ->
            if not caching then begin
              caching <- true;
              self#log#info "Enabling caching mode: %s." msg
            end

    (* Ask for initialization.
     * The current implementation makes it dangerous to call #get_ready from
     * another thread than the Root one, as interleaving with #get is
     * forbidden. *)
    method get_ready ?(dynamic = false) (activation : operator list) =
      if log == source_log then self#create_log;
      if static_activations = [] && dynamic_activations = [] then begin
        source_log#info "Source %s gets up." id;
        self#wake_up activation;
        if commands <> [] then begin
          assert (ns = []);
          ns <- Server.register [self#id] ns_kind;
          self#set_id (Server.to_string ns);
          List.iter
            (fun (descr, usage, name, f) -> Server.add ~ns ~descr ?usage name f)
            commands
        end
      end;
      if dynamic then dynamic_activations <- activation :: dynamic_activations
      else static_activations <- activation :: static_activations;
      self#update_caching_mode;
      let clock_id, clock_sync_mode =
        match deref self#clock with
          | Known c -> (c#id, (c#sync_mode :> clock_sync_mode))
          | _ -> ("unknown", `Unknown)
      in
      self#iter_watchers (fun w ->
          w.get_ready ~stype:self#stype ~is_output:self#is_output ~id:self#id
            ~content_kind:self#kind ~clock_id ~clock_sync_mode)

    (* Release the source, which will shutdown if possible.
     * The current implementation makes it dangerous to call #leave from
     * another thread than the Root one, as interleaving with #get is
     * forbidden. *)
    method leave ?(dynamic = false) src =
      let rec remove acc = function
        | [] ->
            self#log#critical "Got ill-balanced activations (from %s)!" src#id;
            assert false
        | (s :: _) :: tl when s = src -> List.rev_append acc tl
        | h :: tl -> remove (h :: acc) tl
      in
      if dynamic then dynamic_activations <- remove [] dynamic_activations
      else static_activations <- remove [] static_activations;
      self#update_caching_mode;
      if static_activations = [] && dynamic_activations = [] then begin
        source_log#info "Source %s gets down." id;
        (Tutils.mutexify on_shutdown_m (fun () ->
             List.iter (fun fn -> try fn () with _ -> ()) on_shutdown;
             on_shutdown <- []))
          ();
        self#sleep;
        List.iter (fun (_, _, name, _) -> Server.remove ~ns name) commands;
        if ns <> [] then begin
          Server.unregister ns;
          ns <- []
        end
      end;
      self#iter_watchers (fun w -> w.leave ())

    method is_up = static_activations <> [] || dynamic_activations <> []

    (** Two methods called for initialization and shutdown of the source *)
    method private wake_up activation =
      self#log#info "Content kind is %s."
        (Frame.string_of_content_kind content_kind);
      let activation = (self :> operator) :: activation in
      List.iter (fun s -> s#get_ready ?dynamic:None activation) sources

    method private sleep =
      List.iter (fun s -> s#leave ?dynamic:None (self :> operator)) sources

    (** Streaming *)

    method kind = content_kind

    (* Number of frames left in the current track:
     * -1 means Infinity, time unit is the frame. *)
    method virtual remaining : int

    (* [self#seek x] skips [x] master ticks.
     * returns the number of ticks actually skipped.
     * By default it always returns 0, refusing to seek at all. *)
    method seek (_ : int) =
      self#log#important "Seek not implemented!";
      0

    (* Is there some data available for the next [get]?
     * Must always be true while playing a track, i.e. all tracks
     * must be properly ended. *)
    method virtual is_ready : bool

    (* If possible, end the current track.
     * Typically, that signal is just re-routed, or makes the next file
     * to be played if there's anything like a file. *)
    method virtual abort_track : unit

    (* In caching mode, remember what has been given during the current tick *)
    val memo = Frame.create content_kind

    method get_memo = memo

    method private instrumented_get_frame buf =
      if watchers = [] then self#get_frame buf
      else begin
        let start_time = Unix.gettimeofday () in
        let start_position = Frame.position buf in
        self#get_frame buf;
        let end_time = Unix.gettimeofday () in
        let end_position = Frame.position buf in
        let is_partial = Frame.is_partial buf in
        let metadata =
          List.filter
            (fun (pos, _) -> start_position <= pos)
            (Frame.get_all_metadata buf)
        in
        self#iter_watchers (fun w ->
            w.get_frame ~start_time ~start_position ~end_time ~end_position
              ~is_partial ~metadata)
      end

    (* [#get buf] completes the frame with the next data in the stream.
     * Depending on whether caching is enabled or not,
     * it calls [#get_frame] directly or tries to get data from the cache frame,
     * filling it if needed.
     * Any source calling [other_source#get should] thus take care of clearing
     * the cache of the other source ([#advance]) at the end of the output
     * round ([#after_output]). *)
    method get buf =
      assert (Frame.is_partial buf);
      (* In some cases we can't avoid #get being called on a non-ready
       * source, for example:
       * - A starts pumping B, stops in the middle of the track
       * - B finishes its track, becomes unavailable
       * - A starts streaming again, needs to receive an EOT before
       *   having to worry about availability.
       *
       *   Another important example is crossfade, if e.g. a transition
       *   returns a failling source.
       *
       * So we add special cases where, instead of calling #get_frame, we
       * call silent_end_track to properly end a track by inserting a break.
       *
       * This makes the whole protocol a bit sloppy as it weakens constraints
       * tying #is_ready and #get, preventing the detection of "bad" calls
       * of #get without prior check of #is_ready.
       *
       * This fix makes it really important to keep #is_ready = true during a
       * track, otherwise the track will be ended without the source noticing! *)
      let silent_end_track () = Frame.add_break buf (Frame.position buf) in
      if not caching then
        if not self#is_ready then silent_end_track ()
        else (
          let b = Frame.breaks buf in
          self#instrumented_get_frame buf;
          if List.length b + 1 <> List.length (Frame.breaks buf) then begin
            self#log#severe "#get_frame didn't add exactly one break!";
            assert false
          end )
      else begin
        try Frame.get_chunk buf memo
        with Frame.No_chunk ->
          if not self#is_ready then silent_end_track ()
          else (
            (* [memo] has nothing new for [buf]. Feed [memo] and try again *)
            let b = Frame.breaks memo in
            let p = Frame.position memo in
            self#instrumented_get_frame memo;
            if List.length b + 1 <> List.length (Frame.breaks memo) then begin
              self#log#severe "#get_frame didn't add exactly one break!";
              assert false
            end
            else if p < Frame.position memo then self#get buf
            else Frame.add_break buf (Frame.position buf) )
      end

    (* That's the way the source produces audio data.
     * It cannot be called directly, but [#get] should be used instead, for
     * dealing with caching if needed. *)
    method virtual private get_frame : Frame.t -> unit

    (* End the current output round.
     * The default task is to clear the cache and propagate the call
     * to children sources.
     * It MUST be called at every frame for every source, even those who
     * aren't played. This clock may be used for consistency of
     * #is_ready/#get_frame for example, stopping the clock for one source
     * can freeze its state in an unwanted way. *)
    method after_output =
      List.iter (fun s -> s#after_output) sources;
      self#advance;
      self#iter_watchers (fun w -> w.after_output ())

    (* Reset the cache frame *)
    method advance = Frame.advance memo

    (** Utils. *)

    (** Creates an audio request and sets the field 'source' to the relevant id
    * in the metadatas of the request. *)
    method private create_request ?(metadata = []) =
      let metadata = ("source", self#id) :: metadata in
      Request.create ~metadata ~kind:content_kind
  end

(** Entry-point sources, which need to actively perform some task. *)
and virtual active_operator ?name content_kind sources =
  object (self)
    inherit operator ?name content_kind sources

    initializer
    has_outputs := true;
    add_new_output (self :> active_operator);
    ignore
      (unify self#clock
         (create_unknown ~sources:[(self :> active_operator)] ~sub_clocks:[]))

    method is_output = true

    (** Start a new output round, may trigger the computation of a frame. *)
    method virtual output : unit

    (** Do whatever needed when the latency gets too big and is reset. *)
    method virtual output_reset : unit

    (** Is the source active ? *)
    method virtual is_active : bool

    (** Special init phase for outputs. This method is called by Root after the
    * standard get_ready propagation, after the Root clock is started.
    * It allows enhancements of the initial latency. *)
    method virtual output_get_ready : unit
  end

(** Shortcuts for defining sources with no children *)

class virtual source ?name content_kind =
  object
    inherit operator ?name content_kind []
  end

class virtual active_source ?name content_kind =
  object
    inherit active_operator ?name content_kind []
  end

(** Specialized shortcuts *)

type clock_variable = active_source var

class type clock =
  object
    method id : string

    method sync_mode : sync

    method attach : active_source -> unit

    method detach : (active_source -> bool) -> unit

    method attach_clock : clock_variable -> unit

    method detach_clock : clock_variable -> unit

    method sub_clocks : clock_variable list

    method start_outputs : (active_source -> bool) -> unit -> active_source list

    method get_tick : int

    method end_tick : unit
  end

module Clock_variables = struct
  let to_string = variable_to_string
  let create_unknown = create_unknown
  let create_known = create_known
  let unify = unify
  let forget = forget
  let get v = match deref v with Known c -> c | _ -> assert false
  let is_known v = match deref v with Known _ -> true | _ -> false
end

let has_outputs () = !has_outputs
