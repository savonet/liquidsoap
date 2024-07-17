(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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

(** Given abstract start stop and send methods, creates an output.
  * Takes care of pulling the data out of the source, type checkings,
  * maintains a queue of last ten metadata and setups standard Server commands,
  * including start/stop. *)
class virtual output ~kind ?(name="") val_source autostart =
  let source = Lang.to_source val_source in
object (self)
  inherit active_operator source as super

  method virtual output_start : unit
  method virtual output_stop : unit
  method virtual output_send : Frame.t -> unit

  initializer
    (* We need the source to be infallible. *)
    if source#stype <> Infallible then
      raise (Lang.Invalid_value (val_source, "That source is fallible"))

  method remaining = source#remaining
  method abort_track = source#abort_track

  val mutable does_output = false       (* Currently outputting *)
  val mutable start_output = autostart  (* Ask for startup *)
  val mutable stop_output = false       (* Ask for termination *)

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
      ns <- Server.register [self#id] kind ;
    self#set_id (Server.to_string ns) ;
    Server.add ~ns "skip" (fun _ -> self#skip ; "Done") ;
    Server.add ~ns "metadatas"
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
    Server.add ~ns "remaining"
      (fun _ ->
         let r = source#remaining in
           if r < 0 then "(undef)" else
             let t = Fmt.seconds_of_ticks r in
               Printf.sprintf "%.2f" t) ;
    Server.add ~ns "start" (fun _ -> start_output <- true ; "") ;
    Server.add ~ns "stop" (fun _ -> stop_output <- true ; "") ;
    Server.add ~ns "status" (fun _ -> if does_output then "on" else "off") ;

    (* Get our source ready *)
    source#get_ready ((self:>operator)::activation) ;
    while not source#is_ready do
      self#log#f 3 "Waiting for %S to be ready..." source#id ;
      Thread.delay 1. ;
    done

  method output_get_ready =
    if start_output then begin
      start_output <- false ;
      if not does_output then self#output_start ;
      does_output <- true
    end

  method private sleep = source#leave (self:>operator)

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
    if start_output then begin
      start_output <- false ;
      if not does_output then self#output_start ;
      does_output <- true
    end ;

    if does_output then begin
      (* Complete filling of the frame *)
      while Frame.is_partial memo do
        source#get memo
      done ;
      List.iter
        (fun (i,m) -> self#add_metadata m)
        (Frame.get_all_metadata memo) ;
      (* Output that frame *)
      self#output_send memo
    end ;

    if stop_output then begin
      stop_output <- false ;
      if does_output then self#output_stop ;
      does_output <- false
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

class dummy source = object
  inherit output ~kind:"output.dummy" source true
  method output_reset  = ()
  method output_start  = ()
  method output_stop   = ()
  method output_send _ = ()
end
let () =
  Lang.add_operator "output.dummy"
    ["",Lang.source_t,None,None]
    ~category:Lang.Output
    ~descr:"Dummy output for debugging purposes."
    (fun p -> ((new dummy (List.assoc "" p)):>Source.source))

(** More concrete abstract-class, which takes care of the #output_send
  * method for outputs based on encoders. *)
class virtual ['a] encoded ~kind ~name ~autostart source =
object (self)
  inherit output ~kind ~name source autostart

  method virtual reset_encoder : 'a -> (string,string) Hashtbl.t -> string
  method virtual encode : 'a -> float array array -> int -> int -> string
  method virtual send : string -> unit

  val mutable encoder : 'a option = None

  method output_send frame =
    let encoder = Utils.get_some encoder in
    let rec output_chunks frame =
      let f start stop =
        begin
          match AFrame.get_metadata frame start with
            | None -> ()
            | Some m ->
                let h = self#reset_encoder encoder m in
                  self#send h
        end ;
        let data =
          self#encode encoder (AFrame.get_float_pcm frame) start (stop-start)
        in
          self#send data
      in
        function
          | [] -> assert false
          | [i] -> assert (i=AFrame.size frame)
          | start::stop::l ->
              if start < stop then f start stop else assert (start=stop) ;
              output_chunks frame (stop::l)
    in
      output_chunks frame
        (0::(List.sort compare
               ((List.map fst (AFrame.get_all_metadata frame))
                @(AFrame.breaks frame))))

end
