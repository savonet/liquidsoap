(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

let log = Log.log ~label:"source"

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
 * It requires that one source is activated only once by another.
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

  (* General information *)

  method is_output = false

  (** Children sources, if any *)
  val mutable sources : source list = []

  method stype = Fallible

  val mutable id = ""
  val mutable definitive_id = false
  initializer
    id <- "src_" ^ (string_of_int (Oo.id self))
  method set_id ?(definitive=true) s =
    if not definitive_id then ( id <- s ; definitive_id <- definitive )
  method id = id

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
          self#log 4 "Disabling caching mode"
        end
      | Some msg -> if not caching then begin
          caching <- true ;
          self#log 4 (Printf.sprintf "Enabling caching mode (%s)" msg)
        end

  (* Ask for initialization.
   * The current implementation makes it dangerous to call #get_ready from
   * another thread than the Root one, as interleaving with #get is
   * forbidden. *)
  method get_ready ?(dynamic=false) activation =
    if static_activations = [] && dynamic_activations = [] then begin
      log 4 (Log.f "%s gets up" id) ;
      self#wake_up activation
    end ;
    if dynamic then
      dynamic_activations <- activation::dynamic_activations
    else
      static_activations <- activation::static_activations ;
    self#update_caching_mode

  method output_get_ready = ()

  (* Release the source, which will shutdown if possible.
   * The current implementation makes it dangerous to call #leave from
   * another thread than the Root one, as interleaving with #get is
   * forbidden. *)
  method leave ?(dynamic=false) src =
    let rec remove acc = function
      | [] -> self#log 1 "Got ill-balanced activations !" ;assert false
      | (s::_)::tl when s = src -> List.rev_append acc tl
      | h::tl -> remove (h::acc) tl
    in
      if dynamic then
        dynamic_activations <- remove [] dynamic_activations
      else
        static_activations <- remove [] static_activations ;
      if static_activations = [] && dynamic_activations = [] then begin
        log 4 (Log.f "%s gets down" id) ;
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
  method remaining = -1

  (* Is there some data available for the next [get] ? *)
  method virtual is_ready : bool

  (* If possible, end the current track.
   * Typically, that signal is just re-routed, or makes the next file
   * to be played if there's anything like a file. *)
  method virtual abort_track : unit

  (* In caching mode, remember what has been given during the current tick *)
  val memo = Mixer.Buffer.create ()

  (* [#get buf] completes the buffer with the next data in the stream.
   * Depending whether caching is enabled or not, it calls [#get_frame] directly
   * or tries to get data from the cache buffer, filling it if needed.
   * Any source calling [other_source#get should] thus take care of clearing
   * the cache of the other source ([#clear_cache]) at the end of the output
   * round ([#after_output]). *)
  method get buf =
    assert (Mixer.Buffer.is_partial buf) ;
    if not caching then begin
      let b = Mixer.Buffer.breaks buf in
        self#get_frame buf ;
        if b = Mixer.Buffer.breaks buf then
          self#log 2 "#get_frame didn't change the buffer !"
    end else begin
      try
        Mixer.Buffer.get_chunk buf memo
      with
      | Mixer.Buffer.No_chunk ->
          (* [memo] has nothing new for [buf]. Feed [memo] and try again *)
          let b = Mixer.Buffer.breaks memo in
          let p = Mixer.Buffer.position memo in
            self#get_frame memo ;
            if b = Mixer.Buffer.breaks memo then
              self#log 2 "#get_frame didn't change the buffer"
            else
              if p < Mixer.Buffer.position memo then self#get buf else
                Mixer.Buffer.add_break buf (Mixer.Buffer.position buf)
    end

  (* That's the way the source produces audio data.
   * It cannot be called directly, but [#get] should be used instead, for
   * dealing with caching if needed. *)
  method private virtual get_frame : Mixer.Buffer.t -> unit

  (* This method is called by the root on Root.registered entry points.
   * For other nodes it is unnecessary. In other words a source is lazy
   * by default, outputs nothing. But some output classes have this method
   * filled with some get_frame stuff which actually runs the scheduler,
   * in order to get frames and use them for output. *)
  method output = ()

  (* End the current output round.
   * The default task is to clear the cache and propagate the call
   * to children sources. *)
  method after_output =
    List.iter (fun s -> s#after_output) sources ;
    self#clear_cache

  (* Reset the cache buffer *)
  method clear_cache =
    Mixer.Buffer.free memo

  (** Utils. *)

  (** Creates an audio request and sets the field 'source' to the relevant id
    * in the metadatas of the request. *)
  method private create_request ?(metadata=[]) =
    let metadata = ("source",self#id)::metadata in
      Request.create ~metadata

  method private log lvl msg =
    Dtools.Log.log ~label:self#id lvl msg
  method private logl lvl msg =
    Dtools.Log.logl ~label:self#id lvl msg

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

(* Another one for defining an output operator. One which actively pulls
 * the stream. *)
class virtual active_operator (l:source) =
object (self)
  inherit source
  initializer
    sources <- [l] ;
    register (self:>source)

  method is_ready = true
  method stype = Infallible
  method is_output = true
end

(* Same without input source *)
class virtual active_source =
object (self)
  inherit source
  initializer
    sources <- [] ;
    register (self:>source)

  method is_ready = true
  method stype = Infallible
  method is_output = true
end
