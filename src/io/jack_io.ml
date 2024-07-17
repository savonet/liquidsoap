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

(** A few things should be shared among several sources. *)

(* TODO: resample to / from Jack.get_sample_rate *)

let need_jack = ref false

module Ringbuffer = Jack.Ringbuffer.Float

let client =
  let client = ref None in
  let setting = "jack.client_name" in
    Dtools.Var.register setting Dtools.Var.String ;
    ignore (Dtools.Init.at_start
              (fun () ->
                 if !need_jack then
                 let name =
                   Dtools.Conf.get_string ~default:"liquidsoap-PID" setting
                 in
                 let name =
                   Str.global_replace
                     (Str.regexp "PID") (string_of_int (Unix.getpid ())) name
                 in
                   Dtools.Log.log ~label:"jack" 3
                     (Printf.sprintf "Creating client %S..." name) ;
                   let jc = Jack.Client.create name in
                     (* TODO: this causes liquidsoap to segfault, we should
                      * investigate why... *)
                     (*
                     Jack.Client.on_shutdown jc
                       (fun () -> failwith "Jack server was shut down!");
                     *)
                     client := Some jc)) ;
    fun () ->
      match !client with
        | None -> assert false
        | Some c -> c

let add_ringbuffer_callback, activate =
  let l = ref [] in
    (fun x -> l := x :: !l),
    let activated = ref false in
      fun () ->
        if not !activated then
          let client = client () in
            Dtools.Log.log ~label:"jack" 3
              (Printf.sprintf "Activating %d port(s).." (List.length !l)) ;
            activated := true ;
            Jack.Client.set_process_ringbuffer_callback client !l ;
            Jack.Client.activate client

(* The ringbuffer between us and the Jack engine, interleaved.
 * The multiplication by 4 is an heuristic for now.. seems to avoid
 * underruns.. TODO *)
let ringbuffer_coeff = 4

class virtual base port_names mode =
object

  val channels = Fmt.channels ()
  val samples_per_second = Fmt.samples_per_second ()
  val samples_per_frame = Fmt.samples_per_frame ()

  initializer need_jack := true

  method remaining = -1
  method abort_track  = ()
  method output_reset = ()

  val mutable ring = [||]
  initializer
    ring <- Array.init
      (Array.length port_names)
      (fun _ -> Ringbuffer.create (ringbuffer_coeff * samples_per_frame))

  val mutable port = [||]

  method output_get_ready =
    let c = client () in
      port <-
      Array.mapi
        (fun i pn ->
           let p = Jack.Client.register_port
                     c pn "32 bit float mono audio"
                     [if mode=`Input then Jack.Port.Input else Jack.Port.Output] 0
           in
             add_ringbuffer_callback
               (p, ring.(i), if mode=`Input then Jack.Client.Write else Jack.Client.Read);
             p
        )
        port_names

end

class input port_names =
object (self)
  inherit Source.active_source
  inherit base port_names `Input

  method output = if AFrame.is_partial memo then self#get_frame memo

  (* TODO: check that we are using the right values
   * (i.e. !Fmt.samples_per_frame vs Array.length dest.(0), etc) *)
  method get_frame ab =
    activate () ;
    (* [ab] should be [memo], a fresh buffer to fill. *)
    assert (0 = AFrame.position ab) ;
    AFrame.add_break ab samples_per_frame ;
    let dest = AFrame.get_float_pcm ab in
      (* TODO: proper error *)
      assert (Array.length port_names <= Array.length dest);

      for chan = 0 to Array.length port_names - 1 do
        (* Ugly synchronization *)
        while Ringbuffer.read_space ring.(chan) < samples_per_frame do
          Thread.delay 0.01
        done ;
        (* Throw data if there's too much... *)
        (* TODO: be more clever *)
        while
          Ringbuffer.read_space ring.(chan) >= (ringbuffer_coeff / 2) * samples_per_frame
        do
          Ringbuffer.read_advance ring.(chan) samples_per_frame;
          self#log 4 ("Dropping some samples from the ringbuffer.")
        done;

        (* Get some float samples and convert them. *)
        let n = Ringbuffer.read ring.(chan) dest.(chan) 0 (Array.length dest.(chan)) in
        let p = Jack.Port.connected port.(chan) in
        let p = if p = 0 then 1. else float p in
          (* Normally we get a full buffer, otherwise we'll leave blanks. *)
          if n <> Array.length dest.(chan) then
            Dtools.Log.log ~label:"jack" 2
              (Printf.sprintf "Port %S: read %d < %d" port_names.(chan) n (Array.length dest.(chan)));
          if p <> 1. then
            for i = 0 to n - 1 do
              dest.(chan).(i) <- dest.(chan).(i) /. p ;
            done
      done
end

class output port_names val_source =
  let source = Lang.to_source val_source in
object (self)
  initializer
    (* We need the source to be infallible. *)
    if source#stype <> Source.Infallible then
      raise (Lang.Invalid_value (val_source, "That source is fallible"))

  inherit Source.active_operator source
  inherit base port_names `Output

  method get_frame ab = source#get ab

  method output =
    (* Pull the stream until we get a full buffer. *)
    (* TODO: is it really necessary? *)
    while AFrame.is_partial memo do
      source#get memo
    done ;
    let s = AFrame.get_float_pcm memo in
      (* TODO: proper error *)
      assert (Array.length port_names <= Array.length s);
      activate () ;

      for chan = 0 to Array.length port_names - 1 do
        while Ringbuffer.write_space ring.(chan) < samples_per_frame do
          Thread.delay 0.01
        done ;
        let n = Ringbuffer.write ring.(chan) s.(chan) 0 (Array.length s.(chan)) in
          if n <> Array.length s.(chan) then
            Dtools.Log.log ~label:"jack" 2
              (Printf.sprintf "Port %S: wrote %d < %d" port_names.(chan) n (Array.length s.(chan)))
      done

end

let rec get_default_ports name n =
  if n = 0 then []
  else
    (Lang.string (name ^ "_" ^ string_of_int (n-1)))::(get_default_ports name (n-1))

let get_default_ports name =
  Lang.list (List.rev (get_default_ports name (Fmt.channels())))

let () =
  Lang.add_operator "input.jack"
    ~category:Lang.Input
    ~descr:"Jack input."
    ~flags:[Lang.Experimental]
    [ "", Lang.list_t Lang.string_t, Some (get_default_ports "input"), Some "Port names" ]
    (fun p ->
       let ports = Lang.to_list (List.assoc "" p) in
       let ports = List.map Lang.to_string ports in
       let ports = Array.of_list ports in
         ((new input ports):>Source.source)) ;
  Lang.add_operator "output.jack"
    ~category:Lang.Output
    ~descr:"Jack output."
    ~flags:[Lang.Experimental]
    [ "", Lang.list_t Lang.string_t, Some (get_default_ports "output"), Some "Port names" ;
      "", Lang.source_t, None, None ]
    (fun p ->
       let ports = Lang.to_list (List.assoc "" p) in
       let ports = List.map Lang.to_string ports in
       let ports = Array.of_list ports in
         ((new output
             ports
             (Lang.assoc "" 2 p)):>Source.source))
