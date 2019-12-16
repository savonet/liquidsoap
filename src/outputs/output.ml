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

(** Abstract classes for easy creation of output nodes. *)

open Source

let proto =
  [
    ( "fallible",
      Lang.bool_t,
      Some (Lang.bool false),
      Some
        "Allow the child source to fail, in which case the output will be \
         (temporarily) stopped." );
    ( "on_start",
      Lang.fun_t [] Lang.unit_t,
      Some (Lang.val_cst_fun [] Lang.unit),
      Some "Callback executed when outputting starts." );
    ( "on_stop",
      Lang.fun_t [] Lang.unit_t,
      Some (Lang.val_cst_fun [] Lang.unit),
      Some "Callback executed when outputting stops." );
    ( "start",
      Lang.bool_t,
      Some (Lang.bool true),
      Some
        "Automatically start outputting whenever possible. If true, an \
         infallible (normal) output will start outputting as soon as it is \
         created, and a fallible output will (re)start as soon as its source \
         becomes available for streaming." );
  ]

(** Given abstract start stop and send methods, creates an output.
  * Takes care of pulling the data out of the source, type checkings,
  * maintains a queue of last ten metadata and setups standard Server commands,
  * including start/stop. *)
class virtual output ~content_kind ~output_kind ?(name = "") ~infallible
  ~(on_start : unit -> unit) ~(on_stop : unit -> unit) val_source autostart =
  let source = Lang.to_source val_source in
  object (self)
    initializer
    (* This should be done before the active_operator initializer
     * attaches us to a clock. *)
    if infallible && source#stype <> Infallible then
      raise (Lang_errors.Invalid_value (val_source, "That source is fallible"))

    inherit active_operator ~name:output_kind content_kind [source] as super

    inherit
      Start_stop.base
        ~name ~source_kind:output_kind ~interactive:true ~on_start ~on_stop
          ~autostart as start_stop

    (* Eventually we can simply rename them... *)
    method private start = self#output_start

    method private stop = self#output_stop

    method virtual private output_start : unit

    method virtual private output_stop : unit

    method virtual private output_send : Frame.t -> unit

    method stype = source#stype

    method self_sync = source#self_sync

    initializer
    (* Add a few more server controls *)
    self#register_command "skip"
      (fun _ ->
        self#skip;
        "Done")
      ~descr:"Skip current song.";
    self#register_command "metadata" ~descr:"Print current metadata." (fun _ ->
        let q = self#metadata_queue in
        fst
          (Queue.fold
             (fun (s, i) m ->
               let s =
                 s
                 ^ (if s = "" then "--- " else "\n--- ")
                 ^ string_of_int i ^ " ---\n"
                 ^ Request.string_of_metadata m
               in
               (s, i - 1))
             ("", Queue.length q)
             q));
    self#register_command "remaining" ~descr:"Display estimated remaining time."
      (fun _ ->
        let r = source#remaining in
        if r < 0 then "(undef)"
        else (
          let t = Frame.seconds_of_master r in
          Printf.sprintf "%.2f" t ))

    method is_ready =
      if infallible then (
        assert source#is_ready;
        true )
      else source#is_ready

    method remaining = source#remaining

    method abort_track = source#abort_track

    method seek len = source#seek len

    (* Operator startup *)
    method private wake_up activation =
      start_stop#wake_up activation;

      (* Get our source ready.
       * This can take a while (preparing playlists, etc). *)
      source#get_ready ((self :> operator) :: activation);
      if infallible then
        while not source#is_ready do
          self#log#important "Waiting for %S to be ready..." source#id;
          Thread.delay 1.
        done

    method private may_start = if self#is_ready then start_stop#may_start

    method output_get_ready = self#may_start

    method private sleep =
      self#do_stop;
      source#leave (self :> operator)

    (* Metadata stuff: keep track of what was streamed. *)
    val q_length = 10

    val metadata_q = Queue.create ()

    method private add_metadata m =
      Queue.add m metadata_q;
      if Queue.length metadata_q > q_length then ignore (Queue.take metadata_q)

    method private metadata_queue = Queue.copy metadata_q

    (* The output process *)
    val mutable skip = false

    method private skip = skip <- true

    method private get_frame buf = source#get buf

    method private output =
      self#may_start;
      if is_started then (
        (* Complete filling of the frame *)
        let get_count = ref 0 in
        while Frame.is_partial memo && self#is_ready do
          incr get_count;
          if !get_count > Lazy.force Frame.size then
            self#log#severe
              "Warning: there may be an infinite sequence of empty tracks!";
          source#get memo
        done;
        List.iter
          (fun (_, m) -> self#add_metadata m)
          (Frame.get_all_metadata memo);

        (* Output that frame if it has some data *)
        if Frame.position memo > 0 then self#output_send memo;
        if Frame.is_partial memo then (
          self#log#important "Source failed (no more tracks) stopping output...";
          request_stop <- true ) );
      self#may_stop

    method after_output =
      (* Let [memo] be cleared and signal propagated *)
      super#after_output;

      (* Perform skip if needed *)
      if skip then (
        self#log#important "Performing user-requested skip";
        skip <- false;
        self#abort_track )
  end

class dummy ~infallible ~on_start ~on_stop ~autostart ~kind source =
  object
    inherit
      output
        source autostart ~name:"dummy" ~output_kind:"output.dummy" ~infallible
          ~on_start ~on_stop ~content_kind:kind

    method private output_reset = ()

    method private output_start = ()

    method private output_stop = ()

    method private output_send _ = ()
  end

let () =
  let kind = Lang.univ_t () in
  Lang.add_operator "output.dummy" ~active:true
    (proto @ [("", Lang.source_t kind, None, None)])
    ~category:Lang.Output ~descr:"Dummy output for debugging purposes."
    ~kind:(Lang.Unconstrained kind)
    (fun p kind ->
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let autostart = Lang.to_bool (List.assoc "start" p) in
      let on_start = List.assoc "on_start" p in
      let on_stop = List.assoc "on_stop" p in
      let on_start () = ignore (Lang.apply ~t:Lang.unit_t on_start []) in
      let on_stop () = ignore (Lang.apply ~t:Lang.unit_t on_stop []) in
      ( new dummy
          ~kind ~on_start ~on_stop ~infallible ~autostart (List.assoc "" p)
        :> Source.source ))

(** More concrete abstract-class, which takes care of the #output_send
  * method for outputs based on encoders. *)
class virtual encoded ~content_kind ~output_kind ~name ~infallible ~on_start
  ~on_stop ~autostart source =
  object (self)
    inherit
      output
        ~infallible ~on_start ~on_stop ~content_kind ~output_kind ~name source
          autostart

    method virtual private insert_metadata : Meta_format.export_metadata -> unit

    method virtual private encode : Frame.t -> int -> int -> 'a

    method virtual private send : 'a -> unit

    method private output_send frame =
      let rec output_chunks frame =
        let f start stop =
          begin
            match Frame.get_metadata frame start with
            | None -> ()
            | Some m -> self#insert_metadata (Meta_format.export_metadata m)
          end;
          let data = self#encode frame start (stop - start) in
          self#send data
        in
        function
        | [] -> assert false
        | [i] -> assert (i = Lazy.force Frame.size || not infallible)
        | start :: stop :: l ->
            if start < stop then f start stop else assert (start = stop);
            output_chunks frame (stop :: l)
      in
      output_chunks frame
        ( 0
        :: List.sort compare
             (List.map fst (Frame.get_all_metadata frame) @ Frame.breaks frame)
        )
  end
