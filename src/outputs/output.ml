(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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

(** Abstract classes for easy creation of output nodes. *)

open Source

let proto = 
 [
  "fallible",
  Lang.bool_t,
  Some (Lang.bool false),
  Some
    "Allow the child source to fail, \
     in which case the output will be (temporarily) stopped." ;

  "on_start",
  Lang.fun_t [] Lang.unit_t,
  Some (Lang.val_cst_fun [] Lang.unit),
  Some "Callback executed when outputting starts." ;

  "on_stop",
  Lang.fun_t [] Lang.unit_t,
  Some (Lang.val_cst_fun [] Lang.unit),
  Some "Callback executed when outputting stops."  ;

  "start",
  Lang.bool_t,
  Some (Lang.bool true),
  Some "Automatically start outputting whenever possible. \
        If true, an infallible (normal) output will start outputting \
        as soon as it is created, \
        and a fallible output will (re)start as soon as its \
        source becomes available for streaming."
 ]

(** Given abstract start stop and send methods, creates an output.
  * Takes care of pulling the data out of the source, type checkings,
  * maintains a queue of last ten metadata and setups standard Server commands,
  * including start/stop. *)
class virtual output ~content_kind ~output_kind ?(name="")
    ~infallible 
    ~(on_start:unit->unit) ~(on_stop:unit->unit)
    val_source autostart =
  let source = Lang.to_source val_source in
object (self)
  inherit active_operator content_kind source as super

  method virtual output_start : unit
  method virtual output_stop : unit
  method virtual output_send : Frame.t -> unit

  initializer
    if infallible && source#stype <> Infallible then
      raise (Lang.Invalid_value (val_source, "That source is fallible"))

  method stype = source#stype

  method is_ready =
    if infallible then begin
      assert (source#is_ready) ;
      true
    end else
      source#is_ready

  method remaining = source#remaining
  method abort_track = source#abort_track

  val mutable does_output = false       (* Currently outputting *)
  val mutable start_output = autostart  (* Ask for startup *)
  val mutable stop_output = false       (* Ask for termination *)
  val mutable autostart = autostart     (* Start as soon as possible *)

  method is_active = does_output

  (* Operator startup *)

  val mutable ns = []
  method private wake_up activation =
    (* Server commands
     * We prefer [name] as an ID over the default,
     * but do not overwrite user-defined ID.
     * Then we get a unique Server identifier,
     * and finally set the ID to be the same. *)
    if name <> "" then self#set_id ~definitive:false name ;
    if ns = [] then
      ns <- Server.register [self#id] output_kind ;
    self#set_id (Server.to_string ns) ;
    self#log#f 4
      "Content kind is %s."
      (Frame.string_of_content_kind content_kind) ;
    Server.add ~ns "skip" (fun _ -> self#skip ; "Done") 
               ~descr:"Skip current song.";
    Server.add ~ns "metadata" ~descr:"Print current metadata."
      (fun _ ->
         let q = self#metadata_queue in
           (fst (Queue.fold
                   (fun (s,i) m ->
                      let s = s^
                              (if s = "" then "--- " else "\n--- ")^
                              (string_of_int i)^" ---\n"^
                              (Request.string_of_metadata m) in
                        s,(i-1))
                   ("",(Queue.length q)) q))) ;
    Server.add ~ns "remaining" ~descr:"Display estimated remaining time."
      (fun _ ->
         let r = source#remaining in
           if r < 0 then "(undef)" else
             let t = Frame.seconds_of_master r in
               Printf.sprintf "%.2f" t) ;
    Server.add ~ns "autostart" ~descr:"Enable/disable autostart."
      (fun s ->
         if s <> "" then begin
           let update = s = "on" || s = "yes" || s = "y" in
             (* Update start_output when:
              *  - autostart becomes true (we now wait to start asap)
              *  - autostart becomes false too (stop ongoing waiting)
              * But not when it is unchanged. For example, this prevents
              * cancelling a manually-ordered start. *)
             if update <> autostart then begin
               start_output <- update ;
               autostart <- update
             end
         end ;
         if autostart then "on" else "off") ;
    Server.add ~ns "start" ~descr:"Start output."
      (fun _ -> start_output <- true ; "OK") ;
    Server.add ~ns "stop" ~descr:"Stop output. Disable autostart."
      (fun _ ->
         if autostart then begin
           autostart <- false ;
           start_output <- false
         end ;
         stop_output <- true ;
         "OK") ;
    Server.add ~ns "status" ~descr:"Get status."
      (fun _ -> if does_output then "on" else "off") ;

    (* Get our source ready.
     * This can take a while (preparing playlists, etc). *)
    source#get_ready ((self:>operator)::activation) ;
    if infallible then
      while not source#is_ready do
        self#log#f 3 "Waiting for %S to be ready..." source#id ;
        Thread.delay 1. ;
      done

  method output_get_ready =
    if start_output && self#is_ready then begin
      start_output <- autostart ;
      if not does_output then begin
        self#output_start ;
        on_start () ;
        does_output <- true
      end
    end

  method private sleep =
    if does_output then begin
      self#output_stop ;
      on_stop () ;
      does_output <- false
    end ;
    source#leave (self:>operator)

  (* Metadata stuff: keep track of what was streamed. *)

  val q_length = 10
  val metadata_q = Queue.create ()

  method add_metadata m =
    Queue.add m metadata_q ;
    if Queue.length metadata_q > q_length then ignore (Queue.take metadata_q) ;

  method metadata_queue = Queue.copy metadata_q

  (* The output process *)

  val mutable skip = false
  method private skip = skip <- true
  method private get_frame buf = source#get buf

  method private output =
    if start_output && self#is_ready then begin
      start_output <- autostart ;
      if not does_output then begin
        self#output_start ;
        on_start () ;
        does_output <- true
      end
    end ;

    if does_output then begin
      (* Complete filling of the frame *)
      while Frame.is_partial memo && self#is_ready do
        source#get memo
      done ;
      List.iter
        (fun (i,m) -> self#add_metadata m)
        (Frame.get_all_metadata memo) ;
      (* Output that frame *)
      self#output_send memo
    end ;

    if stop_output || Frame.is_partial memo then begin
      stop_output <- false ;
      if does_output then begin
        if Frame.is_partial memo then
          self#log#f 3 "Source failed (no more tracks), output stops." ;
        self#output_stop ;
        on_stop () ;
        does_output <- false
      end
    end

  method after_output =
    (* Let [memo] be cleared and signal propagated *)
    super#after_output ;
    (* Perform skip if needed *)
    if skip then begin
      self#log#f 3 "Performing user-requested skip" ;
      skip <- false ;
      self#abort_track
    end

end

class dummy ~infallible ~on_start ~on_stop ~autostart ~kind source =
object
  inherit
    output source autostart
      ~name:"dummy" ~output_kind:"output.dummy"
      ~infallible ~on_start ~on_stop
      ~content_kind:kind

  method output_reset  = ()
  method output_start  = ()
  method output_stop   = ()
  method output_send _ = ()
end

let () =
  let kind = Lang.univ_t 1 in
  Lang.add_operator "output.dummy"
    (proto @ ["", Lang.source_t kind, None, None])
    ~category:Lang.Output
    ~descr:"Dummy output for debugging purposes."
    ~kind:(Lang.Unconstrained kind)
    (fun p kind ->
       let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
       let autostart = Lang.to_bool (List.assoc "start" p) in
       let on_start = List.assoc "on_start" p in
       let on_stop = List.assoc "on_stop" p in
       let on_start () = ignore (Lang.apply ~t:Lang.unit_t on_start []) in
       let on_stop () = ignore (Lang.apply ~t:Lang.unit_t on_stop []) in
         ((new dummy ~kind ~on_start ~on_stop ~infallible ~autostart
             (List.assoc "" p)):>Source.source))

(** More concrete abstract-class, which takes care of the #output_send
  * method for outputs based on encoders. *)
class virtual encoded
  ~content_kind ~output_kind ~name
  ~infallible ~on_start ~on_stop
  ~autostart source =
object (self)
  inherit output
            ~infallible ~on_start ~on_stop
            ~content_kind ~output_kind ~name source autostart

  method virtual reset_encoder : (string,string) Hashtbl.t -> string
  method virtual encode : Frame.t -> int -> int -> string
  method virtual send : string -> unit

  method output_send frame =
    let rec output_chunks frame =
      let f start stop =
        begin
          match Frame.get_metadata frame start with
            | None -> ()
            | Some m ->
                let h = self#reset_encoder m in
                  self#send h
        end ;
        let data =
          self#encode frame start (stop-start)
        in
          self#send data
      in
        function
          | [] -> assert false
          | [i] -> assert (i=Lazy.force Frame.size || not infallible)
          | start::stop::l ->
              if start < stop then f start stop else assert (start=stop) ;
              output_chunks frame (stop::l)
    in
      output_chunks frame
        (0::(List.sort compare
               ((List.map fst (Frame.get_all_metadata frame))
                @(Frame.breaks frame))))

end
