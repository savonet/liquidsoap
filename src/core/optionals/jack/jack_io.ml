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

let conf_jack =
  Dtools.Conf.void ~p:(Configure.conf#plug "jack") "JACK configuration"

let conf_latency =
  Dtools.Conf.float ~p:(conf_jack#plug "latency") ~d:0.
    "How much time ahead (in seconds) we should be until we let the streaming \
     loop rest."

let conf_max_latency =
  Dtools.Conf.float
    ~p:(conf_jack#plug "max_latency")
    ~d:0.5 "Maximum latency in seconds"

module ServerState = struct
  type t

  external create : unit -> t = "caml_jack_server_state_create"

  external set_stopped : t -> bool -> unit
    = "caml_jack_server_state_set_stopped"
  [@@noalloc]

  external get_stopped : t -> bool = "caml_jack_server_state_get_stopped"
  external get_elapsed : t -> float = "caml_jack_server_state_get_elapsed"

  external set_sleep_target : t -> float -> unit
    = "caml_jack_server_state_set_sleep_target"
  [@@noalloc]

  external wait : t -> unit = "caml_jack_server_state_wait"
end

module JackSource = struct
  type t

  external create : unit -> t = "caml_jack_source_create"

  external set_client : t -> Jack.client -> unit = "caml_jack_source_set_client"
  [@@noalloc]

  external set_server_state : t -> ServerState.t -> unit
    = "caml_jack_source_set_server_state"
  [@@noalloc]

  external register_callback : Jack.client -> t -> unit
    = "caml_jack_source_register_callback"

  external add_port : t -> Jack.port -> bool -> int
    = "caml_jack_source_add_port"

  external enable_port : t -> int -> Jack.Ringbuffer.t -> unit
    = "caml_jack_source_enable_port"
  [@@noalloc]

  external disable_port : t -> int -> unit = "caml_jack_source_disable_port"
  [@@noalloc]

  external remove_port : t -> int -> unit = "caml_jack_source_remove_port"
  [@@noalloc]

  external get_and_reset_dropped : t -> int -> int
    = "caml_jack_source_get_and_reset_dropped"
  [@@noalloc]
end

module SyncSource = Clock.MkSyncSource (struct
  type t = < id : string ; time_implementation : Liq_time.implementation >

  let time_implementation s = s#time_implementation
  let to_string s = Printf.sprintf "jack(%s)" s#id
  let latency _ = conf_latency#get
  let max_latency _ = conf_max_latency#get
end)

module Time = (val Liq_time.unix : Liq_time.T)

let make_time_impl server_state : Liq_time.implementation =
  (module struct
    include Time

    let time () = Time.of_float (ServerState.get_elapsed server_state)

    let sleep_until target =
      if ServerState.get_stopped server_state then raise Clock.Has_stopped;
      let target_f = Time.to_float target in
      if ServerState.get_elapsed server_state < target_f then begin
        ServerState.set_sleep_target server_state target_f;
        ServerState.wait server_state
      end;
      if ServerState.get_stopped server_state then raise Clock.Has_stopped
  end)

type server_data = {
  server_state : ServerState.t;
  sync_source : Clock.sync_source;
}

let server_data_list : (string option * server_data) list ref = ref []
let server_data_state = Mutex_utils.mk_state ()

let get_server_data server =
  Mutex_utils.atomic_lock ~state:server_data_state
    (fun () ->
      match List.assoc_opt server !server_data_list with
        | Some data -> data
        | None ->
            let server_state = ServerState.create () in
            let time_impl = make_time_impl server_state in
            let sync_src =
              SyncSource.make
                (object
                   method id = Option.value ~default:"default" server
                   method time_implementation = time_impl
                end)
            in
            let data = { server_state; sync_source = sync_src } in
            server_data_list := (server, data) :: !server_data_list;
            data)
    ()

let () =
  Lifecycle.before_core_shutdown ~name:"jack" (fun () ->
      Mutex_utils.atomic_lock ~state:server_data_state
        (fun () ->
          List.iter
            (fun (_, data) -> ServerState.set_stopped data.server_state true)
            !server_data_list)
        ())

class jack_ringbuffer ~sample_rate () =
  let max_latency = conf_max_latency#get in
  let bytes =
    int_of_float (Float.ceil (max_latency *. float sample_rate *. 4.))
  in
  let ringbuffer = Jack.Ringbuffer.create bytes in
  let () = Jack.Ringbuffer.mlock ringbuffer in
  object
    method raw = ringbuffer

    method write_from_ba buf ofs nframes =
      Jack.Ringbuffer.write_from_ba ringbuffer buf ofs nframes

    method write_from_buffer buf ofs len =
      Jack.Ringbuffer.write_from_buffer ringbuffer buf ofs len

    method write_space = Jack.Ringbuffer.write_space ringbuffer / 4

    method read_to_ba buf ofs nframes =
      Jack.Ringbuffer.read_to_ba ringbuffer buf ofs nframes

    method read_alloc = Jack.Ringbuffer.read_alloc ringbuffer

    method read_to_buffer buf ofs len =
      Jack.Ringbuffer.read_to_buffer ringbuffer buf ofs len

    method read_space = Jack.Ringbuffer.read_space ringbuffer / 4

    method flush =
      Jack.Ringbuffer.read_advance ringbuffer
        (Jack.Ringbuffer.read_space ringbuffer)
  end

class jack_port ~source ~is_input ~(unregister : unit -> unit) ~sample_rate
  (port : Jack.port) =
  let index = JackSource.add_port source port is_input in
  object (self)
    inherit jack_ringbuffer ~sample_rate ()
    method port = port
    method enable = JackSource.enable_port source index self#raw
    method disable = JackSource.disable_port source index
    method get_and_reset_dropped = JackSource.get_and_reset_dropped source index

    method unregister =
      JackSource.remove_port source index;
      unregister ()
  end

class jack_client ~id (server : string option) =
  let liq_rate = Lazy.force Frame.audio_rate in
  object
    val mutable client : Jack.client option = None
    val mutable activated = false
    val mutable buffer_size = 0
    val mutable sample_rate = 0
    method id = id
    method client = client
    method sample_rate = sample_rate
    method buffer_size = buffer_size

    method open_client =
      let options =
        `NoStartServer
        :: Option.fold ~none:[] ~some:(fun s -> [`ServerName s]) server
      in
      let c =
        try Jack.client_open id options
        with Failure msg -> Runtime_error.raise ~pos:[] ~message:msg "jack"
      in
      client <- Some c;
      let r = Jack.get_sample_rate c in
      if r <> liq_rate then
        log#important
          "JACK sample rate %d differs from liquidsoap rate %d, resampling." r
          liq_rate;
      sample_rate <- r;
      buffer_size <- Jack.get_buffer_size c

    method activate =
      if not activated then (
        Jack.activate (Option.get client);
        activated <- true)

    method close =
      Option.iter Jack.client_close client;
      client <- None;
      activated <- false

    method register_port ~source ~is_input ?port_type ?buffer_size name =
      match client with
        | None -> failwith "jack_client: not connected"
        | Some c ->
            let flags = if is_input then [`IsInput] else [`IsOutput] in
            let p = Jack.port_register ?port_type ?buffer_size c name flags in
            let unregister () =
              match client with
                | None -> ()
                | Some c -> Jack.port_unregister c p
            in
            new jack_port ~source ~is_input ~unregister ~sample_rate p

    method main_of_frames frames =
      int_of_float
        (Float.floor
           (float frames
           *. float (Lazy.force Frame.audio_rate)
           /. float sample_rate))

    method frames_of_main ticks =
      int_of_float
        (Float.floor
           (float ticks *. float sample_rate
           /. float (Lazy.force Frame.audio_rate)))
  end

class virtual base ~server () =
  let { server_state; sync_source } = get_server_data server in
  let samples_per_second = Lazy.force Frame.audio_rate in
  object (self)
    method virtual log : Log.t
    method virtual audio_channels : int
    method virtual id : string
    method virtual on_wake_up : (unit -> unit) -> unit
    method virtual on_sleep : (unit -> unit) -> unit
    method virtual private is_input : bool
    method virtual on_start : (unit -> unit) -> unit
    method virtual on_stop : (unit -> unit) -> unit
    val mutable _jack_client : jack_client option = None
    val mutable _jack_source : JackSource.t option = None

    val mutable samplerate_converter : Audio_converter.Samplerate.t option =
      None

    val mutable ports = [||]
    method private jack_client : jack_client = Option.get _jack_client

    method private clear_jack_client =
      Option.iter (fun c -> c#close) _jack_client;
      _jack_client <- None

    method self_sync : Clock.self_sync =
      ( `Dynamic,
        match _jack_client with None -> None | Some _ -> Some sync_source )

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

    method private start =
      let frame_duration = Lazy.force Frame.duration in
      let jack_client = self#jack_client in
      let n_samples =
        int_of_float (frame_duration *. float jack_client#sample_rate)
      in
      (* Add one JACK buffer of padding to cover reads and writes that land in
         the middle of a JACK buffer. This can be skipped when the sample rates
         match and the frame size is already an exact multiple of the JACK
         buffer — there is no mid-buffer misalignment in that case. *)
      let aligned =
        jack_client#sample_rate = samples_per_second
        && n_samples mod jack_client#buffer_size = 0
      in
      let n =
        if aligned then n_samples else n_samples + jack_client#buffer_size
      in
      let silence = Array.make n 0. in
      Array.iter (fun p -> ignore (p#write_from_buffer silence 0 n)) ports

    method private stop = Array.iter (fun p -> p#flush) ports

    initializer
      self#on_wake_up (fun () ->
          let jack_client = new jack_client ~id:self#id server in
          _jack_client <- Some jack_client;
          jack_client#open_client;
          let src = JackSource.create () in
          JackSource.set_client src (Option.get jack_client#client);
          JackSource.set_server_state src server_state;
          JackSource.register_callback (Option.get jack_client#client) src;
          jack_client#activate;
          _jack_source <- Some src;
          let is_input = self#is_input in
          ports <-
            Array.init self#audio_channels (fun i ->
                let name = Printf.sprintf "%s_%d" self#id i in
                jack_client#register_port ~source:src ~is_input name));
      self#on_start (fun () -> Array.iter (fun p -> p#enable) ports);
      self#on_stop (fun () -> Array.iter (fun p -> p#disable) ports);
      self#on_sleep (fun () ->
          _jack_source <- None;
          Array.iter (fun p -> p#unregister) ports;
          ports <- [||];
          self#clear_jack_client)
  end

class input ~server ~autostart =
  object (self)
    inherit
      Start_stop.active_source ~name:"input.jack" ~fallible:false ~autostart () as active_source

    inherit base ~server ()
    method effective_source = (self :> Source.source)

    method private can_generate_frame =
      if active_source#started then begin
        self#drain_ringbuffer;
        Generator.length self#buffer >= Lazy.force Frame.size
      end
      else false

    method abort_track = ()
    method remaining = -1
    method private is_input = true

    method private drain_ringbuffer =
      Array.iter
        (fun p ->
          let dropped = p#get_and_reset_dropped in
          if dropped > 0 then
            self#log#important "input overrun: %.3gs"
              (float dropped /. float self#jack_client#sample_rate))
        ports;
      let audio =
        Array.init self#audio_channels (fun ch -> ports.(ch)#read_alloc)
      in
      let len =
        Array.fold_left (fun m arr -> max m (Array.length arr)) 0 audio
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
        let audio, offset, length = self#resample_from_jack audio len in
        let content =
          Content.Audio.lift_data
            ~offset:(Frame.main_of_audio offset)
            ~length:(Frame.main_of_audio length)
            audio
        in
        Generator.put self#buffer Frame.Fields.audio content
      end

    method private generate_frame =
      let frame_size = Lazy.force Frame.size in
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

    inherit base ~server ()
    method private is_input = false

    method private feed_ringbuffer =
      Array.iter
        (fun p ->
          let dropped = p#get_and_reset_dropped in
          if dropped > 0 then
            self#log#important "output underrun: %.3gs"
              (float dropped /. float self#jack_client#sample_rate))
        ports;
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
      Array.iteri
        (fun ch port ->
          let n = port#write_from_buffer buf.(ch) ofs len in
          if n <> len then
            self#log#info "output: wrote %d instead of %d jack frames" n len)
        ports

    method send_frame frame =
      Generator.append self#buffer frame;
      self#feed_ringbuffer;
      (* With next_usecs-based wakeup we have a full JACK period to write, so
         the ringbuffer should always have space. Log if anything remains. *)
      if Generator.length self#buffer > 0 then
        self#log#important "output: ringbuffer full, %d samples pending"
          (Generator.length self#buffer)
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
