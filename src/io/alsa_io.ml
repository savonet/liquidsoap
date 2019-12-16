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

open Alsa

exception Error of string

let handle lbl f x =
  try f x
  with e ->
    failwith
      (Printf.sprintf "Error while setting %s: %s" lbl (string_of_error e))

class virtual base ~kind dev mode =
  let channels = (Frame.type_of_kind kind).Frame.audio in
  let samples_per_second = Lazy.force Frame.audio_rate in
  let samples_per_frame = AFrame.size () in
  let periods = Alsa_settings.periods#get in
  object (self)
    method virtual log : Log.t

    val mutable alsa_rate = -1

    val mutable pcm = None

    val mutable write =
      fun pcm buf ofs len ->
        let buf = Array.map (fun buf -> Bigarray.Array1.sub buf ofs len) buf in
        Pcm.writen_float_ba pcm buf

    val mutable read =
      fun pcm buf ofs len ->
        let buf = Array.map (fun buf -> Bigarray.Array1.sub buf ofs len) buf in
        Pcm.readn_float_ba pcm buf

    method self_sync = pcm <> None

    method open_device =
      self#log#important "Using ALSA %s." (Alsa.get_version ());
      try
        let dev =
          match pcm with
            | None -> handle "open_pcm" (Pcm.open_pcm dev mode) []
            | Some d -> d
        in
        let params = Pcm.get_params dev in
        ( try
            handle "access"
              (Pcm.set_access dev params)
              Pcm.Access_rw_noninterleaved;
            handle "format" (Pcm.set_format dev params) Pcm.Format_float
          with _ -> (
            (* If we can't get floats we fallback on interleaved s16le *)
            self#log#important "Falling back on interleaved S16LE";
            handle "format" (Pcm.set_format dev params) Pcm.Format_s16_le;
            try
              Pcm.set_access dev params Pcm.Access_rw_interleaved;
              write <-
                (fun pcm buf ofs len ->
                  let sbuf = Bytes.create (2 * len * Array.length buf) in
                  Audio.S16LE.of_audio (Audio.sub buf ofs len) sbuf 0;
                  Pcm.writei pcm (Bytes.unsafe_to_string sbuf) 0 len);
              read <-
                (fun pcm buf ofs len ->
                  let sbuf = String.make (2 * 2 * len) (Char.chr 0) in
                  let r = Pcm.readi pcm sbuf 0 len in
                  Audio.S16LE.to_audio sbuf 0 (Audio.sub buf ofs r);
                  r)
            with Alsa.Invalid_argument ->
              self#log#important "Falling back on non-interleaved S16LE";
              handle "access"
                (Pcm.set_access dev params)
                Pcm.Access_rw_noninterleaved;
              write <-
                (fun pcm buf ofs len ->
                  let sbuf =
                    Array.init channels (fun _ ->
                        String.make (2 * len) (Char.chr 0))
                  in
                  for c = 0 to Audio.channels buf - 1 do
                    Audio.S16LE.of_audio
                      (Audio.sub [| buf.(c) |] ofs len)
                      (Bytes.of_string sbuf.(c))
                      0
                  done;
                  Pcm.writen pcm sbuf 0 len);
              read <-
                (fun pcm buf ofs len ->
                  let sbuf =
                    Array.init channels (fun _ ->
                        String.make (2 * len) (Char.chr 0))
                  in
                  let r = Pcm.readn pcm sbuf 0 len in
                  for c = 0 to Audio.channels buf - 1 do
                    Audio.S16LE.to_audio sbuf.(c) 0
                      (Audio.sub [| buf.(c) |] ofs len)
                  done;
                  r) ) );
        handle "channels" (Pcm.set_channels dev params) channels;
        let rate =
          handle "rate" (Pcm.set_rate_near dev params samples_per_second) Dir_eq
        in
        let bufsize =
          handle "buffer size"
            (Pcm.set_buffer_size_near dev params)
            samples_per_frame
        in
        let periods =
          if periods > 0 then (
            handle "periods" (Pcm.set_periods dev params periods) Dir_eq;
            periods )
          else fst (Pcm.get_periods_max params)
        in
        alsa_rate <- rate;
        if rate <> samples_per_second then
          self#log#important
            "Could not set sample rate to 'frequency' (%d Hz), got %d."
            samples_per_second rate;
        if bufsize <> samples_per_frame then
          self#log#important
            "Could not set buffer size to 'frame.size' (%d samples), got %d."
            samples_per_frame bufsize;
        self#log#important "Samplefreq=%dHz, Bufsize=%dB, Frame=%dB, Periods=%d"
          alsa_rate bufsize
          (Pcm.get_frame_size params)
          periods;
        ( try Pcm.set_params dev params
          with Alsa.Invalid_argument as e ->
            self#log#critical
              "Setting alsa parameters failed (invalid argument)!";
            raise e );
        handle "non-blocking" (Pcm.set_nonblock dev) false;
        pcm <- Some dev
      with Unknown_error _ as e -> raise (Error (string_of_error e))

    method close_device =
      match pcm with
        | Some d ->
            Pcm.close d;
            pcm <- None
        | None -> ()

    method output_reset =
      self#close_device;
      self#open_device
  end

class output ~kind ~clock_safe ~start ~infallible ~on_stop ~on_start dev
  val_source =
  let channels = (Frame.type_of_kind kind).Frame.audio in
  let samples_per_second = Lazy.force Frame.audio_rate in
  let name = Printf.sprintf "alsa_out(%s)" dev in
  object (self)
    inherit
      Output.output
        ~infallible ~on_stop ~on_start ~content_kind:kind ~name
          ~output_kind:"output.alsa" val_source start as super

    inherit base ~kind dev [Pcm.Playback]

    method private set_clock =
      super#set_clock;
      if clock_safe then
        Clock.unify self#clock
          (Clock.create_known (Alsa_settings.get_clock () :> Clock.clock))

    val samplerate_converter = Audio_converter.Samplerate.create channels

    method output_start = self#open_device

    method output_stop = self#close_device

    method output_send memo =
      let pcm = Utils.get_some pcm in
      let buf = AFrame.content memo 0 in
      let buf =
        if alsa_rate = samples_per_second then buf
        else
          Audio_converter.Samplerate.resample samplerate_converter
            (float alsa_rate /. float samples_per_second)
            buf
      in
      try
        let r = ref 0 in
        while !r < Audio.Mono.length buf.(0) do
          if !r <> 0 then
            self#log#info
              "Partial write (%d instead of %d)! Selecting another buffer size \
               or device can help."
              !r
              (Audio.Mono.length buf.(0));
          r := !r + write pcm buf !r (Audio.Mono.length buf.(0) - !r)
        done
      with e ->
        begin
          match e with
          | Buffer_xrun ->
              self#log#severe
                "Underrun! You may minimize them by increasing the buffer size."
          | _ -> self#log#severe "Alsa error: %s" (string_of_error e)
        end;
        if e = Buffer_xrun || e = Suspended || e = Interrupted then (
          self#log#severe "Trying to recover..";
          Pcm.recover pcm e;
          self#output )
        else raise e
  end

class input ~kind ~clock_safe ~start ~on_stop ~on_start ~fallible dev =
  let channels = (Frame.type_of_kind kind).Frame.audio in
  let samples_per_frame = AFrame.size () in
  object (self)
    inherit base ~kind dev [Pcm.Capture]

    inherit
      Start_stop.input
        ~content_kind:kind ~source_kind:"alsa"
        ~name:(Printf.sprintf "alsa_in(%s)" dev)
        ~on_start ~on_stop ~fallible ~autostart:start as super

    method private set_clock =
      super#set_clock;
      if clock_safe then
        Clock.unify self#clock
          (Clock.create_known (Alsa_settings.get_clock () :> Clock.clock))

    method private start = self#open_device

    method private stop = self#close_device

    (* TODO: convert samplerate *)
    method private input frame =
      let pcm = Utils.get_some pcm in
      let buf = AFrame.content_of_type ~channels frame 0 in
      try
        let r = ref 0 in
        while !r < samples_per_frame do
          if !r <> 0 then
            self#log#info
              "Partial read (%d instead of %d)! Selecting another buffer size \
               or device can help."
              !r (Audio.length buf);
          r := !r + read pcm buf !r (samples_per_frame - !r)
        done;
        AFrame.add_break frame (AFrame.size ())
      with e ->
        begin
          match e with
          | Buffer_xrun ->
              self#log#severe
                "Overrun! You may minimize them by increasing the buffer size."
          | _ -> self#log#severe "Alsa error: %s" (string_of_error e)
        end;
        if e = Buffer_xrun || e = Suspended || e = Interrupted then (
          self#log#severe "Trying to recover..";
          Pcm.recover pcm e;
          self#output )
        else raise e
  end

let () =
  let k = Lang.kind_type_of_kind_format (Lang.any_fixed_with ~audio:1 ()) in
  Lang.add_operator "output.alsa" ~active:true
    ( Output.proto
    @ [
        ( "bufferize",
          Lang.bool_t,
          Some (Lang.bool true),
          Some "Bufferize output" );
        ( "clock_safe",
          Lang.bool_t,
          Some (Lang.bool true),
          Some "Force the use of the dedicated ALSA clock" );
        ( "device",
          Lang.string_t,
          Some (Lang.string "default"),
          Some "Alsa device to use" );
        ("", Lang.source_t k, None, None);
      ] )
    ~kind:(Lang.Unconstrained k) ~category:Lang.Output
    ~descr:"Output the source's stream to an ALSA output device."
    (fun p kind ->
      let e f v = f (List.assoc v p) in
      let bufferize = e Lang.to_bool "bufferize" in
      let clock_safe = e Lang.to_bool "clock_safe" in
      let device = e Lang.to_string "device" in
      let source = List.assoc "" p in
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let start = Lang.to_bool (List.assoc "start" p) in
      let on_start =
        let f = List.assoc "on_start" p in
        fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
      in
      let on_stop =
        let f = List.assoc "on_stop" p in
        fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
      in
      if bufferize then
        ( new Alsa_out.output
            ~kind ~clock_safe ~start ~on_start ~on_stop ~infallible device
            source
          :> Source.source )
      else
        ( new output
            ~kind ~clock_safe ~infallible ~start ~on_start ~on_stop device
            source
          :> Source.source ))

let () =
  let k = Lang.kind_type_of_kind_format Lang.audio_any in
  Lang.add_operator "input.alsa" ~active:true
    ( Start_stop.input_proto
    @ [
        ("bufferize", Lang.bool_t, Some (Lang.bool true), Some "Bufferize input");
        ( "clock_safe",
          Lang.bool_t,
          Some (Lang.bool true),
          Some "Force the use of the dedicated ALSA clock" );
        ( "device",
          Lang.string_t,
          Some (Lang.string "default"),
          Some "Alsa device to use" );
      ] )
    ~kind:(Lang.Unconstrained k) ~category:Lang.Input
    ~descr:"Stream from an ALSA input device."
    (fun p kind ->
      let e f v = f (List.assoc v p) in
      let bufferize = e Lang.to_bool "bufferize" in
      let clock_safe = e Lang.to_bool "clock_safe" in
      let device = e Lang.to_string "device" in
      let start = Lang.to_bool (List.assoc "start" p) in
      let fallible = Lang.to_bool (List.assoc "fallible" p) in
      let on_start =
        let f = List.assoc "on_start" p in
        fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
      in
      let on_stop =
        let f = List.assoc "on_stop" p in
        fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
      in
      if bufferize then
        (new Alsa_in.mic ~kind ~clock_safe device :> Source.source)
      else
        ( new input ~kind ~clock_safe ~on_start ~on_stop ~fallible ~start device
          :> Source.source ))
