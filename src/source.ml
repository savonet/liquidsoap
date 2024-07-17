(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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

open Dtools

let source_log = Log.make ["source"]

type source_t = Fallible | Infallible

(* Activation management.
 *
 * A source may be accessed by several sources, and must switch to caching
 * mode when it may be accessed by more than one source, in order to ensure
 * the consistency of the delivered audio data.
 *
 * Before that a source P accesses another source S it must activate it. The
 * activation can be static, or dynamic. A dynamic activation means that P won't
 * use S directly but may at some point build a dynamic source which will access
 * S. It is important that such possibility is known by S, since it can happen
 * in the middle of an output round, in which case S will have to be in caching
 * mode if accessed at the same time by another source.
 *
 * An activation is identified by the path to the source which required it.
 * It is possible that two identical activations are done, and they should not
 * be treated as a single one.
 *
 * In short, a source can avoid caching when: it has only one static activation
 * and all its dynamic activations are sub-paths of the static one.
 * When there is no static activation, there cannot be any access.
 *
 * A dynamic activation allows more than one static activations to happen
 * at runtime, which requires a lock to avoid that the first registered
 * source gets data before the second one is registered and makes the accessed
 * source switch to caching mode.
 * TODO This has not been implemented, since all calls to #get, #get_ready and
 * #leave are done in the same Root thread. *)

class virtual source =
object (self)

  val mutable log = source_log
  method private create_log = log <- Log.make [self#id]
  method private log = log

  (* General information *)
  method is_output = false

  (** Children sources, if any *)
  val mutable sources : source list = []

  method stype = Fallible

  val mutable id = ""
  val mutable definitive_id = false
  initializer
    id <- "src_" ^ (string_of_int (Oo.id self))
  method id = id
  method set_id ?(definitive=true) s =
    (* ID musn't contain "." *)
    let s = Pcre.substitute ~pat:"\\." ~subst:(fun _ -> "(dot)") s in
    if not definitive_id then ( id <- s ; definitive_id <- definitive ) ;
    (* Sometimes the ID is changed during initialization,
     * in order to make it equal to the server name,
     * which is only registered at initialization time in order
     * to avoid bloating from unused sources.
     * If the ID changes, and [log] has already been initialized, reset it. *)
    if log != source_log then self#create_log

  (** Startup/shutdown.
    * Get the source ready for streaming on demand, have it release resources
    * when it's not used any more, and decide whether the source should run in
    * caching mode. *)

  val mutable caching = false
  val mutable dynamic_activations : source list list = []
  val mutable static_activations  : source list list = []

  method private update_caching_mode =
    match
      (* Decide whether caching mode is needed, and why *)
      if self#is_output then Some "active source" else
        match static_activations with
          | [] -> None
          | [s] ->
              if List.exists
                   (fun d -> not (Utils.prefix (List.rev d) (List.rev s)))
                   dynamic_activations then
                Some "possible dynamic activation"
              else
                None
          | _ -> Some "two static activations"
    with
      | None -> if caching then begin
          caching <- false ;
          self#log#f 4 "Disabling caching mode"
        end
      | Some msg -> if not caching then begin
          caching <- true ;
          self#log#f 4 "Enabling caching mode (%s)" msg
        end

  (* Ask for initialization.
   * The current implementation makes it dangerous to call #get_ready from
   * another thread than the Root one, as interleaving with #get is
   * forbidden. *)
  method get_ready ?(dynamic=false) activation =
    if log == source_log then self#create_log ;
    if static_activations = [] && dynamic_activations = [] then begin
      source_log#f 4 "%s gets up" id ;
      self#wake_up activation
    end ;
    if dynamic then
      dynamic_activations <- activation::dynamic_activations
    else
      static_activations <- activation::static_activations ;
    self#update_caching_mode

  (* Release the source, which will shutdown if possible.
   * The current implementation makes it dangerous to call #leave from
   * another thread than the Root one, as interleaving with #get is
   * forbidden. *)
  method leave ?(dynamic=false) src =
    let rec remove acc = function
      | [] -> self#log#f 1 "Got ill-balanced activations !" ; assert false
      | (s::_)::tl when s = src -> List.rev_append acc tl
      | h::tl -> remove (h::acc) tl
    in
      if dynamic then
        dynamic_activations <- remove [] dynamic_activations
      else
        static_activations <- remove [] static_activations ;
      if static_activations = [] && dynamic_activations = [] then begin
        source_log#f 4 "%s gets down" id ;
        self#sleep
      end ;
      self#update_caching_mode

  (** Two methods called for initialization and shutdown of the source *)
  method private wake_up activation =
    let activation = (self:>source)::activation in
      List.iter
        (fun s ->
           (s#get_ready:?dynamic:bool -> source list -> unit) activation)
        sources
  method private sleep =
    List.iter
      (fun s -> (s#leave:?dynamic:bool->source->unit) (self:>source))
      sources

  (** Streaming *)

  (* Number of frames left in the current track:
   * -1 means Infinity, time unit is the frame. *)
  method virtual remaining : int

  (* Is there some data available for the next [get] ? *)
  method virtual is_ready : bool

  (* If possible, end the current track.
   * Typically, that signal is just re-routed, or makes the next file
   * to be played if there's anything like a file. *)
  method virtual abort_track : unit

  (* In caching mode, remember what has been given during the current tick *)
  val memo = Fmt.create_frame ()

  (* [#get buf] completes the frame with the next data in the stream.
   * Depending whether caching is enabled or not, it calls [#get_frame] directly
   * or tries to get data from the cache frame, filling it if needed.
   * Any source calling [other_source#get should] thus take care of clearing
   * the cache of the other source ([#advance]) at the end of the output
   * round ([#after_output]). *)
  method get buf =
    assert (Frame.is_partial buf) ;
    if not caching then begin
      let b = Frame.breaks buf in
        self#get_frame buf ;
        if b = Frame.breaks buf then begin
          self#log#f 2 "#get_frame didn't change the frame!" ;
          assert false
        end
    end else begin
      try
        Frame.get_chunk buf memo
      with
      | Frame.No_chunk ->
          (* [memo] has nothing new for [buf]. Feed [memo] and try again *)
          let b = Frame.breaks memo in
          let p = Frame.position memo in
            self#get_frame memo ;
            if b = Frame.breaks memo then begin
              self#log#f 2 "#get_frame didn't change the frame" ;
              assert false
            end else
              if p < Frame.position memo then self#get buf else
                Frame.add_break buf (Frame.position buf)
    end

  (* That's the way the source produces audio data.
   * It cannot be called directly, but [#get] should be used instead, for
   * dealing with caching if needed. *)
  method private virtual get_frame : Frame.t -> unit

  (* End the current output round.
   * The default task is to clear the cache and propagate the call
   * to children sources.
   * It MUST be called at every frame for every source, even those who
   * aren't played. This clock may be used for consistency of
   * #is_ready/#get_frame for example, stopping the clock for one source
   * can freeze its state in an unwanted way. *)
  method after_output =
    List.iter (fun s -> s#after_output) sources ;
    self#advance

  (* Reset the cache frame *)
  method advance =
    Frame.advance memo

  (** Utils. *)

  (** Creates an audio request and sets the field 'source' to the relevant id
    * in the metadatas of the request. *)
  method private create_request ?(metadata=[]) =
    let metadata = ("source",self#id)::metadata in
      Request.create ~metadata

end

(* Just an easy shortcut for defining the children sources *)
class virtual operator (l:source list) =
object
  inherit source
  initializer sources <- l
end

(** Output stuff. The entry points for the scheduler are the active nodes. *)
let entries = ref []
let register s = entries := s::!entries
let iter_outputs f = List.iter f !entries
let has_outputs () = !entries <> []

(* Entry-points sources, which need to actively perform some task. *)
class virtual active_source =
object (self)
  inherit source
  initializer
    sources <- [] ;
    register (self:>active_source)

  method is_ready = true
  method stype = Infallible

  method is_output = true

  (** Start a new output round, triggers computation of a new frame. *)
  method virtual output : unit

  (** Do whatever needed when the latency gets too big and is reset. *)
  method virtual output_reset : unit

  (** Special init phase for outputs. This method is called by Root after the
    * standard get_ready propagation, after the Root clock is started.
    * It allows enhancements of the initial latency. *)
  method virtual output_get_ready : unit
end

(* Most usual active source: the active_operator, pulling one source's data
 * and outputting it. *)
class virtual active_operator (l:source) =
object (self)
  inherit active_source
  initializer sources <- [l]
end
