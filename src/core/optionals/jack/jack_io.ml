(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

let log = Log.make ["jack"]
let stopped = Atomic.make false

let () =
  Lifecycle.before_core_shutdown ~name:"jack" (fun () ->
      Atomic.set stopped true)

module SyncSource = Clock.MkSyncSource (struct
  type t = < id : string ; time_implementation : Liq_time.implementation >

  let time_implementation s = s#time_implementation
  let to_string s = Printf.sprintf "jack(%s)" s#id
  let max_latency _ = 0.
end)

module Time = (val Liq_time.unix : Liq_time.T)

class jack_ringbuffer ~sample_rate () =
  let frame_duration = Lazy.force Frame.duration in
  let bytes =
    int_of_float (Float.ceil (2.5 *. frame_duration *. float sample_rate *. 4.))
  in
  let ringbuffer = Jack.Ringbuffer.create bytes in
  let () = Jack.Ringbuffer.mlock ringbuffer in
  object
    method write_from_ba buf ofs nframes =
      Jack.Ringbuffer.write_from_ba ringbuffer buf ofs nframes

    method write_from_buffer buf ofs len =
      Jack.Ringbuffer.write_from_buffer ringbuffer buf ofs len

    method write_space = Jack.Ringbuffer.write_space ringbuffer / 4

    method read_to_ba buf ofs nframes =
      Jack.Ringbuffer.read_to_ba ringbuffer buf ofs nframes

    method read_to_buffer buf ofs len =
      Jack.Ringbuffer.read_to_buffer ringbuffer buf ofs len

    method read_space = Jack.Ringbuffer.read_space ringbuffer / 4

    method flush =
      Jack.Ringbuffer.read_advance ringbuffer
        (Jack.Ringbuffer.read_space ringbuffer)
  end

class jack_port ~(unregister : unit -> unit) ~sample_rate (port : Jack.port) =
  object
    inherit jack_ringbuffer ~sample_rate ()
    method port = port
    method unregister = unregister ()
  end

class jack_client (server : string option) =
  let liq_rate = Lazy.force Frame.audio_rate in
  let time_elapsed = Atomic.make 0. in
  let sleep_target = Atomic.make Float.infinity in
  let drift_ema = Atomic.make 0. in
  let ema_alpha = 0.1 in
  let waiter = Jack.Wait.create () in
  object (self)
    val users_count = Atomic.make 0
    val mutable client : Jack.client option = None
    val mutable activated = false
    val process_handlers : (int -> unit) list Atomic.t = Atomic.make []
    val mutable sync_source = None
    val mutable on_release : unit -> unit = Fun.id
    val mutable buffer_size = 0
    val mutable sample_rate = 0
    method set_on_release f = on_release <- f

    val time_implementation : Liq_time.implementation =
      (module struct
        include Time

        let time () = Time.of_float (Atomic.get time_elapsed)

        let sleep_until target =
          let target_f = Time.to_float target in
          let corrected = target_f -. Atomic.get drift_ema in
          if Atomic.get time_elapsed < corrected then begin
            Atomic.set sleep_target corrected;
            Jack.Wait.wait waiter
          end;
          let actual = Atomic.get time_elapsed in
          let drift = actual -. target_f in
          let prev = Atomic.get drift_ema in
          Atomic.set drift_ema (prev +. (ema_alpha *. (drift -. prev)))
      end)

    method time_implementation = time_implementation

    method sync_source =
      match sync_source with
        | Some s -> s
        | None ->
            let s =
              SyncSource.make
                (self
                  :> < id : string
                     ; time_implementation : Liq_time.implementation >)
            in
            sync_source <- Some s;
            s

    method id = Option.value ~default:"default" server
    method client = client
    method sample_rate = sample_rate
    method buffer_size = buffer_size
    method retain = Atomic.incr users_count

    method release =
      if Atomic.fetch_and_add users_count (-1) = 1 then (
        Option.iter Jack.client_close client;
        client <- None;
        activated <- false;
        on_release ())

    method register_process_callback cb =
      let rec loop () =
        let cur = Atomic.get process_handlers in
        if not (Atomic.compare_and_set process_handlers cur (cb :: cur)) then
          loop ()
      in
      loop ()

    method unregister_process_callback cb =
      let rec loop () =
        let cur = Atomic.get process_handlers in
        let next = List.filter (fun c -> c != cb) cur in
        if not (Atomic.compare_and_set process_handlers cur next) then loop ()
      in
      loop ()

    method open_client =
      Atomic.set time_elapsed 0.;
      Atomic.set sleep_target Float.infinity;
      Atomic.set drift_ema 0.;
      let options =
        `NoStartServer
        :: Option.fold ~none:[] ~some:(fun s -> [`ServerName s]) server
      in
      let c = Jack.client_open "liquidsoap" options in
      client <- Some c;
      let r = Jack.get_sample_rate c in
      if r <> liq_rate then
        log#important
          "JACK sample rate %d differs from liquidsoap rate %d, resampling." r
          liq_rate;
      sample_rate <- r;
      buffer_size <- Jack.get_buffer_size c;
      let rec advance dt =
        let cur = Atomic.get time_elapsed in
        if not (Atomic.compare_and_set time_elapsed cur (cur +. dt)) then
          advance dt
      in
      Jack.set_process_callback c (fun nframes ->
          if sample_rate > 0 then advance (float nframes /. float sample_rate);
          List.iter (fun cb -> cb nframes) (Atomic.get process_handlers);
          let target = Atomic.get sleep_target in
          if Atomic.get time_elapsed >= target then begin
            Atomic.set sleep_target Float.infinity;
            Jack.Wait.signal waiter
          end)

    method activate =
      if not activated then (
        Jack.activate (Option.get client);
        activated <- true)

    method register_port ?port_type ?buffer_size name flags =
      match client with
        | None -> failwith "jack_client: not connected"
        | Some c ->
            let p = Jack.port_register ?port_type ?buffer_size c name flags in
            let unregister () =
              match client with
                | None -> ()
                | Some c -> Jack.port_unregister c p
            in
            new jack_port ~unregister ~sample_rate p

    method main_of_frames frames =
      Frame.main_of_seconds (float frames /. float sample_rate)

    method frames_of_main ticks =
      int_of_float (Frame.seconds_of_main ticks *. float sample_rate)
  end

let clients : jack_client list ref = ref []
let client_state = Mutex_utils.mk_state ()

let remove_client c =
  Mutex_utils.atomic_lock ~state:client_state
    (fun () -> clients := List.filter (fun x -> x != c) !clients)
    ()

let get_client server =
  Mutex_utils.atomic_lock ~state:client_state
    (fun () ->
      let id = Option.value ~default:"default" server in
      match List.find_opt (fun c -> c#id = id) !clients with
        | Some c -> c
        | None ->
            let c = new jack_client server in
            c#open_client;
            c#set_on_release (fun () -> remove_client c);
            clients := c :: !clients;
            c)
    ()

class virtual jack_base ~server () =
  let samples_per_second = Lazy.force Frame.audio_rate in
  object (self)
    method virtual log : Log.t
    method virtual audio_channels : int
    method virtual id : string
    method virtual private process_callback : int -> unit
    method virtual on_wake_up : (unit -> unit) -> unit
    method virtual on_sleep : (unit -> unit) -> unit
    method virtual private register_ports : jack_client -> jack_port array
    val mutable _jack_client : jack_client option = None

    val mutable samplerate_converter : Audio_converter.Samplerate.t option =
      None

    val mutable ports = [||]

    method private jack_client =
      match _jack_client with
        | Some e -> e
        | None ->
            let e = get_client server in
            _jack_client <- Some e;
            e#retain;
            e

    method private clear_jack_client =
      Option.iter (fun e -> e#release) _jack_client;
      _jack_client <- None

    method self_sync : Clock.self_sync =
      ( `Dynamic,
        match _jack_client with None -> None | Some c -> Some c#sync_source )

    method private samples_per_second = samples_per_second

    method private samplerate_converter =
      match samplerate_converter with
        | Some sc -> sc
        | None ->
            let sc = Audio_converter.Samplerate.create self#audio_channels in
            samplerate_converter <- Some sc;
            sc

    method private resample_from_jack buf nframes =
      Audio_converter.Samplerate.resample self#samplerate_converter
        (float samples_per_second /. float self#jack_client#sample_rate)
        buf 0 nframes

    method private resample_to_jack buf ticks =
      Audio_converter.Samplerate.resample self#samplerate_converter
        (float self#jack_client#sample_rate /. float samples_per_second)
        buf 0
        (Frame.audio_of_main ticks)

    val mutable process_cb : (int -> unit) option = None

    method private start =
      let frame_duration = Lazy.force Frame.duration in
      let jack_client = self#jack_client in
      let n =
        int_of_float (frame_duration *. float jack_client#sample_rate)
        + jack_client#buffer_size
      in
      let silence = Array.make n 0. in
      Array.iter (fun p -> ignore (p#write_from_buffer silence 0 n)) ports

    method private stop = Array.iter (fun p -> p#flush) ports

    initializer
      self#on_wake_up (fun () ->
          let jack_client = self#jack_client in
          let cb = fun n -> self#process_callback n in
          process_cb <- Some cb;
          jack_client#register_process_callback cb;
          jack_client#activate;
          ports <- self#register_ports jack_client);
      self#on_sleep (fun () ->
          Option.iter
            (fun cb ->
              Option.iter
                (fun c -> c#unregister_process_callback cb)
                _jack_client)
            process_cb;
          process_cb <- None;
          Array.iter (fun p -> p#unregister) ports;
          ports <- [||];
          self#clear_jack_client)
  end

class input ~server ~autostart =
  object (self)
    inherit
      Start_stop.active_source ~name:"input.jack" ~fallible:false ~autostart () as active_source

    inherit jack_base ~server ()
    method effective_source = (self :> Source.source)
    method private can_generate_frame = active_source#started
    method abort_track = ()
    method remaining = -1

    method private process_callback nframes =
      if state = `Started then (
        let n =
          Array.fold_left
            (fun n port ->
              let jack_buf = Jack.port_get_buffer port#port nframes in
              let n' = port#write_from_ba jack_buf 0 nframes in
              min n n')
            nframes ports
        in
        if n < nframes && not (Atomic.get stopped) then
          self#log#important "input overrun: %.3fs"
            (float (nframes - n) /. float self#jack_client#sample_rate))

    method private register_ports entry =
      Array.init self#audio_channels (fun i ->
          entry#register_port (Printf.sprintf "%s_%d" self#id i) [`IsInput])

    method private drain_ringbuffer =
      let audio =
        Array.init self#audio_channels (fun ch ->
            let available = ports.(ch)#read_space in
            let buf = Array.make available 0. in
            let n = ports.(ch)#read_to_buffer buf 0 available in
            if n <> available then
              self#log#info "input: read %d instead of %d jack frames" n
                available;
            buf)
      in
      let len =
        Array.fold_left
          (fun m arr -> max m (Array.length arr))
          (Array.length audio.(0))
          audio
      in
      if len > 0 then begin
        let audio =
          Array.map
            (fun arr ->
              let n = Array.length arr in
              if n < len then Array.append arr (Array.make (len - n) 0.)
              else arr)
            audio
        in
        let audio, ofs, len = self#resample_from_jack audio len in
        let content =
          Content.Audio.lift_data
            (Array.init self#audio_channels (fun ch ->
                 Array.sub audio.(ch) ofs len))
        in
        Generator.put self#buffer Frame.Fields.audio content
      end

    method private generate_frame =
      let frame_size = Lazy.force Frame.size in
      self#drain_ringbuffer;
      (* Busy-wait for a full frame — only expected to spin for a few samples
         at most when the JACK callback hasn't fully filled the buffer yet. *)
      while Generator.length self#buffer < frame_size do
        Domain.cpu_relax ();
        self#drain_ringbuffer
      done;
      Generator.slice self#buffer frame_size
  end

class output ~server ~infallible ~register_telnet source =
  object (self)
    inherit
      Output.output
        ~infallible ~register_telnet ~name:"output.jack"
          ~output_kind:"output.jack" source true

    inherit jack_base ~server ()

    method private feed_ringbuffer =
      let space =
        Array.fold_left
          (fun m port -> min m port#write_space)
          ports.(0)#write_space ports
      in
      let ticks = self#jack_client#main_of_frames space in
      let frame = Generator.slice self#buffer ticks in
      let buf, ofs, len =
        self#resample_to_jack (AFrame.pcm frame) (Frame.position frame)
      in
      let len = min space len in
      Array.iteri
        (fun ch port ->
          let n = port#write_from_buffer buf.(ch) ofs len in
          if n <> len then
            self#log#info "output: wrote %d instead of %d jack frames" n len)
        ports

    method private process_callback nframes =
      match self#state with
        | `Started ->
            if Array.exists (fun port -> port#read_space < nframes) ports then
              self#feed_ringbuffer;
            let n =
              Array.fold_left
                (fun n port ->
                  let jack_buf = Jack.port_get_buffer port#port nframes in
                  let n' = port#read_to_ba jack_buf 0 nframes in
                  for i = n to nframes - 1 do
                    jack_buf.{i} <- 0.
                  done;
                  min n n')
                nframes ports
            in
            if n < nframes && not (Atomic.get stopped) then
              self#log#important "output underrun: %.3fs"
                (float (nframes - n) /. float self#jack_client#sample_rate)
        | _ ->
            Array.iter
              (fun port ->
                let jack_buf = Jack.port_get_buffer port#port nframes in
                if self#state <> `Started then Bigarray.Array1.fill jack_buf 0.)
              ports

    method private register_ports entry =
      Array.init self#audio_channels (fun i ->
          entry#register_port (Printf.sprintf "%s_%d" self#id i) [`IsOutput])

    method send_frame frame =
      Generator.append self#buffer frame;
      self#feed_ringbuffer
  end

let jack_proto =
  [
    ( "server",
      Lang.nullable_t Lang.string_t,
      Some Lang.null,
      Some "JACK server to connect to." );
  ]

let parse_jack_params p =
  let server =
    match Lang.to_valued_option Lang.to_string (List.assoc "server" p) with
      | Some "" -> None
      | s -> s
  in
  server

let _ =
  let return_t =
    Lang.frame_t Lang.unit_t
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator ~base:Modules.input "jack"
    (Start_stop.active_source_proto ~fallible_opt:`Nope @ jack_proto)
    ~meth:(Start_stop.meth ())
    ~callbacks:(Start_stop.callbacks ~label:"source")
    ~return_t ~category:`Input ~descr:"Get stream from JACK."
    (fun p ->
      let autostart = Lang.to_bool (List.assoc "start" p) in
      let server = parse_jack_params p in
      new input ~server ~autostart)

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator ~base:Modules.output "jack"
    (Output.proto @ jack_proto @ [("", Lang.source_t frame_t, None, None)])
    ~return_t:frame_t ~category:`Output ~meth:(Start_stop.meth ())
    ~callbacks:(Start_stop.callbacks ~label:"output")
    ~descr:"Output stream to JACK."
    (fun p ->
      let source = List.assoc "" p in
      let server = parse_jack_params p in
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let register_telnet = Lang.to_bool (List.assoc "register_telnet" p) in
      new output ~server ~infallible ~register_telnet source)
