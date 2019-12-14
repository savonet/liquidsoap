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

(** Base class for sources with start/stop methods and server commands.
  * That class provides #may/do_start/stop but does not hook them anywhere. *)
class virtual base ~name ~source_kind ~interactive ~(on_start : unit -> unit)
  ~(on_stop : unit -> unit) ~autostart =
  object (self)
    method virtual private id : string

    method virtual private set_id : ?definitive:bool -> string -> unit

    method virtual private log : Log.t

    method virtual private start : unit

    method virtual private stop : unit

    val mutable is_started = false

    (* Currently started *)
    val mutable request_start = autostart

    (* Ask for startup *)
    val mutable request_stop = false

    (* Ask for termination *)
    val mutable autostart = autostart

    (* Start as soon as possible *)
    method virtual register_command
        : descr:string -> ?usage:string -> string -> (string -> string) -> unit

    val virtual mutable ns_kind : string

    initializer
    if interactive then (
      ns_kind <- source_kind ;
      self#register_command "autostart" ~descr:"Enable/disable autostart."
        (fun s ->
          if s <> "" then (
            let update = s = "on" || s = "yes" || s = "y" in
            (* Update request_start when:
             *  - autostart becomes true (we now wait to start asap)
             *  - autostart becomes false too (stop ongoing waiting)
             * But not when it is unchanged. For example, this prevents
             * cancelling a manually-ordered start. *)
            if update <> autostart then (
              request_start <- update ;
              autostart <- update ;
              self#notify ) ) ;
          if autostart then "on" else "off") ;
      self#register_command "start" ~descr:"Start." (fun _ ->
          request_start <- true ;
          self#notify ;
          "OK") ;
      self#register_command "stop" ~descr:"Stop and disable autostart."
        (fun _ ->
          if autostart then (
            autostart <- false ;
            request_start <- false ) ;
          request_stop <- true ;
          self#notify ;
          "OK") ;
      self#register_command "status" ~descr:"Get status." (fun _ ->
          if is_started then "on" else "off") )

    method is_active = is_started

    method private wake_up (_ : Source.source list) =
      (* We prefer [name] as an ID over the default,
       * but do not overwrite user-defined ID.
       * Our ID will be used for the server interface. *)
      if name <> "" then self#set_id ~definitive:false name

    method private notify = ()

    method private may_start = if request_start then self#do_start

    method private may_stop = if request_stop then self#do_stop

    method private do_start =
      request_start <- autostart ;
      if not is_started then (
        self#start ;
        on_start () ;
        is_started <- true )

    method private do_stop =
      if is_started then (
        self#stop ;
        on_stop () ;
        is_started <- false ;
        request_stop <- false )
  end

(* Takes care of calling #start/#stop in a well-parenthesized way,
 * but they may be called from any thread.
 * It is suitable for inactive inputs, where the #start/stop methods
 * should start some sort of feeding thread. *)
class virtual async ~name ~source_kind ~(on_start : unit -> unit)
  ~(on_stop : unit -> unit) ~autostart =
  object (self)
    inherit
      base ~name ~source_kind ~interactive:true ~on_start ~on_stop ~autostart as super

    method private wake_up activation =
      super#wake_up activation ; self#may_start

    method private sleep = self#do_stop

    method private notify =
      (* TODO we should avoid race conditions here *)
      self#may_stop ; self#may_start
  end

(** The [input] class should be used for defining active inputs.
  * It only requires #start, #stop and #input methods,
  * and provides start/stop server commands.
  * Currently the start/stop mechanism is always enabled, so the input
  * is fallible.
  * The code is similar to Output.output, with #may_(start/stop) called
  * from the #output method. *)
class virtual input ~name ~source_kind ~content_kind ~(on_start : unit -> unit)
  ~(on_stop : unit -> unit) ~fallible ~autostart =
  object (self)
    inherit Source.active_source ~name:source_kind content_kind

    inherit
      base
        ~name ~source_kind ~interactive:fallible ~on_start ~on_stop ~autostart as super

    method stype = if fallible then Source.Fallible else Source.Infallible

    method output_get_ready =
      if fallible then self#may_start else self#do_start

    method private wake_up activation =
      super#wake_up activation ;
      if fallible then self#may_start else self#do_start

    method private sleep = self#do_stop

    method is_ready =
      if fallible then self#is_active
      else (
        assert self#is_active ;
        true )

    method remaining = if self#is_active then -1 else 0

    method abort_track = ()

    method private output =
      self#may_start ;
      if is_started && AFrame.is_partial memo then self#get_frame memo ;
      if fallible then self#may_stop

    method private get_frame frame =
      (* Because we're an active source, the frame will actually be our memo.
       * Hence we'll always start filling it at position 0, and we'll fill
       * it completely. *)
      assert (0 = AFrame.position frame) ;
      if not is_started then Frame.add_break frame (Frame.position frame)
      else self#input frame

    method virtual private input : Frame.t -> unit
  end

let input_proto =
  [ ( "fallible",
      Lang.bool_t,
      Some (Lang.bool false),
      Some
        "Allow the input to stop. When false, the source will be infallible \
         but the stop command won't have any effect." );
    ( "on_start",
      Lang.fun_t [] Lang.unit_t,
      Some (Lang.val_cst_fun [] Lang.unit),
      Some "Callback executed when input starts." );
    ( "on_stop",
      Lang.fun_t [] Lang.unit_t,
      Some (Lang.val_cst_fun [] Lang.unit),
      Some "Callback executed when input stops." );
    ( "start",
      Lang.bool_t,
      Some (Lang.bool true),
      Some
        "Start input as soon as it is created. Disabling it is only taken \
         into account for a fallible input." ) ]
