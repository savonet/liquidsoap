(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

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

(** Base class for sources with start/stop methods and server commands.
  * That class provides #may/do_start/stop but does not hook them anywhere. *)
class virtual base ?(name="") ~source_kind
  ~(on_start:unit->unit) ~(on_stop:unit->unit) ~autostart =
object (self)

  method virtual private id : string
  method virtual private set_id : ?definitive:bool -> string -> unit
  method virtual private log : Dtools.Log.t

  method virtual private start : unit
  method virtual private stop : unit

  val mutable is_started = false        (* Currently started *)
  val mutable request_start = autostart (* Ask for startup *)
  val mutable request_stop  = false     (* Ask for termination *)
  val mutable autostart = autostart     (* Start as soon as possible *)

  method virtual register_command : descr:string ->
                                    ?usage:string -> string ->
                                    (string->string) -> unit
  val virtual mutable ns_kind : string

  initializer
    ns_kind <- source_kind ;
    (* TODO self#log#f 4
      "Content kind is %s."
      (Frame.string_of_content_kind content_kind) ; *) 
    self#register_command "autostart" ~descr:"Enable/disable autostart."
      (fun s -> 
         if s <> "" then begin
           let update = s = "on" || s = "yes" || s = "y" in
             (* Update request_start when:
              *  - autostart becomes true (we now wait to start asap)
              *  - autostart becomes false too (stop ongoing waiting)
              * But not when it is unchanged. For example, this prevents
              * cancelling a manually-ordered start. *)
             if update <> autostart then begin
               request_start <- update ;
               autostart <- update ;
               self#notify
             end
         end ;
         if autostart then "on" else "off") ;
    self#register_command "start" ~descr:"Start."
      (fun _ -> request_start <- true ; self#notify ; "OK") ;
    self#register_command "stop" ~descr:"Stop and disable autostart."
      (fun _ ->
         if autostart then begin
           autostart <- false ;
           request_start <- false
         end ;
         request_stop <- true ;
         self#notify ;
         "OK") ;
    self#register_command "status" ~descr:"Get status."
      (fun _ -> if is_started then "on" else "off")

  method is_active = is_started

  method private wake_up (activation : Source.source list) =
    (* {Server commands}
     * We prefer [name] as an ID over the default,
     * but do not overwrite user-defined ID.
     * Then we get a unique Server identifier,
     * and finally set the ID to be the same. *)
    if name <> "" then self#set_id ~definitive:false name 

  method private notify = ()

  method private may_start =
    if request_start then self#do_start

  method private may_stop =
    if request_stop then self#do_stop

  method private do_start =
    request_start <- autostart ;
    if not is_started then begin
      self#start ;
      on_start () ;
      is_started <- true
    end

  method private do_stop =
    if is_started then begin
      self#stop ;
      on_stop () ;
      is_started <- false ;
      request_stop <- false
    end

end

(* Takes care of calling #start/#stop in a well-parenthesized way,
 * but they may be called from any thread. *)
class virtual async ?name ~source_kind
  ~(on_start:unit->unit) ~(on_stop:unit->unit) ~autostart =
object (self)

  inherit base ?name ~source_kind ~on_start ~on_stop ~autostart as super

  method private wake_up activation =
    super#wake_up activation ;
    self#may_start

  method private sleep =
    self#do_stop

  method private notify =
    (* TODO we should avoid race conditions here *)
    self#may_stop ;
    self#may_start

end
