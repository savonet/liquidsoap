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

(** Abstract classes for easy creation of output nodes. *)

open Types

class dummy source = object
  inherit active_operator source
  method abort_track = source#abort_track  
  method get_frame buf = source#get buf
  method private output =
    while Mixer.Buffer.is_partial memo do
      let b = Mixer.Buffer.breaks memo in
        source#get memo ;
        assert (b <> Mixer.Buffer.breaks memo) ;
        assert (not (Mixer.Buffer.ill memo))
    done
end
let _ =
  Lang.add_operator "output.dummy"
    ["",Lang.source_t,None,None]
    ~descr:"Dummy output for debugging purposes."
    (fun p -> ((new dummy (Lang.to_source (List.assoc "" p))):>Types.source))

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
  method virtual output_send : Mixer.Buffer.t -> unit

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
  method wake_up activation =
    (* Server commands *)
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
             let t = (float r) *. Mixer.Buffer.length in
               Printf.sprintf "%d sec" (int_of_float t)) ;
    Server.add ~ns "start" (fun _ -> start_output <- true ; "") ;
    Server.add ~ns "stop" (fun _ -> stop_output <- true ; "") ;
    Server.add ~ns "status" (fun _ -> if does_output then "on" else "off") ;

    (* Get our source ready *)
    source#get_ready ((self:>operator)::activation) ;
    while not source#is_ready do
      Dtools.Log.log ~label:"output" 3
        (Printf.sprintf "Waiting for %S to be ready..." source#id) ;
      Thread.delay 1. ;
    done

  method output_get_ready =
    if start_output then begin
      start_output <- false ;
      if not does_output then self#output_start ;
      does_output <- true
    end

  method sleep = source#leave (self:>operator)

  (* Metadata stuff: keep track of what was streamed. *)

  val q_length = 10
  val metadata_q = Queue.create ()

  method add_metadata m =
    Queue.add m metadata_q ;
    if Queue.length metadata_q > q_length then ignore (Queue.take metadata_q) ;

  method metadata_queue = Queue.copy metadata_q

  (* The output process *)

  val mutable skip = false
  method skip = skip <- true
  method get_frame buf = source#get buf

  method private output =
    if start_output then begin
      start_output <- false ;
      if not does_output then self#output_start ;
      does_output <- true
    end ;

    if does_output then begin
      (* Complete filling of the frame *)
      while Mixer.Buffer.is_partial memo do
        let b = Mixer.Buffer.breaks memo in
          source#get memo ;
          assert (b <> Mixer.Buffer.breaks memo) ;
          assert (not (Mixer.Buffer.ill memo))
      done ;
      List.iter
        (fun (i,m) -> self#add_metadata m)
        (Mixer.Buffer.get_all_metadata memo) ;
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
      self#log 3 "Performing user-requested skip" ;
      skip <- false ;
      self#abort_track
    end

end

(** More concrete abstract-class, which takes care of the #output_send
  * method for outputs based on encoders. *)
class virtual ['a] encoded ~kind ~name ~autostart source =
object (self)
  inherit output ~kind ~name source autostart

  method virtual reset_encoder : 'a -> (string,string) Hashtbl.t -> string
  method virtual encode : 'a -> string -> int -> int -> string
  method virtual send : string -> unit

  val mutable encoder : 'a option = None

  method output_send wav =
    let encoder = match encoder with None -> assert false | Some e -> e in
    let rec output_chunks wav =
      let f start stop =
        begin
          match Mixer.Buffer.get_metadata wav start with
            | None -> ()
            | Some m ->
                let h = self#reset_encoder encoder m in
                  self#send h
        end ;
        let data =
          self#encode encoder (Mixer.Buffer.to_string wav) start (stop-start)
        in
          self#send data
      in
        function
          | [] -> assert false
          | [i] -> assert (i=Mixer.Buffer.size)
          | start::stop::l ->
              if start < stop then f start stop else assert (start=stop) ;
              output_chunks wav (stop::l)
    in
      output_chunks wav
        (0::(List.sort compare
               ((List.map fst (Mixer.Buffer.get_all_metadata wav))
                @(Mixer.Buffer.breaks wav))))

end
