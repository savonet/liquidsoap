(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

open Mm

(* Buffered ALSA output *)

open Alsa

class output ~clock_safe ~infallible ~on_stop ~on_start ~start dev source =
  let buffer_length = AFrame.size () in
  let alsa_buffer = Alsa_settings.alsa_buffer#get in
  let nb_blocks = Alsa_settings.conf_buffer_length#get in
  let samples_per_second = Lazy.force Frame.audio_rate in
  let periods = Alsa_settings.periods#get in
  let name = Printf.sprintf "alsa_out(%s)" dev in
  object (self)
    inherit
      Output.output
        ~infallible ~on_stop ~on_start ~name ~output_kind:"output.alsa" source
          start as super

    inherit [Content.Audio.data] IoRing.output ~nb_blocks as ioring
    val mutable initialized = false

    method! wake_up a =
      super#wake_up a;
      let blank () = Audio.make self#audio_channels buffer_length 0. in
      ioring#init blank

    method! private set_clock =
      super#set_clock;
      if clock_safe then
        Clock.unify self#clock
          (Clock.create_known (Alsa_settings.get_clock () :> Clock.clock))

    val mutable device = None
    method! self_sync = (`Dynamic, device <> None)
    val mutable alsa_rate = samples_per_second
    val mutable samplerate_converter = None

    method samplerate_converter =
      (* Defer the creation of the converter until we know the kind and thus the number of channels. *)
      match samplerate_converter with
        | Some samplerate_converter -> samplerate_converter
        | None ->
            let sc = Audio_converter.Samplerate.create self#audio_channels in
            samplerate_converter <- Some sc;
            sc

    val mutable alsa_write =
      fun pcm buf ofs len -> Pcm.writen_float pcm buf ofs len

    method get_device =
      match device with
        | Some d -> d
        | None ->
            self#log#important "Using ALSA %s." (Alsa.get_version ());
            let dev = Pcm.open_pcm dev [Pcm.Playback] [] in
            let params = Pcm.get_params dev in
            let channels = self#audio_channels in
            let bufsize, periods =
              (try
                 Pcm.set_access dev params Pcm.Access_rw_noninterleaved;
                 Pcm.set_format dev params Pcm.Format_float
               with _ ->
                 (* If we can't get floats we fallback on interleaved s16le *)
                 self#log#severe "Falling back on interleaved S16LE";
                 Pcm.set_access dev params Pcm.Access_rw_interleaved;
                 Pcm.set_format dev params Pcm.Format_s16_le;
                 alsa_write <-
                   (fun pcm buf ofs len ->
                     let sbuf =
                       Bytes.create (Audio.S16LE.size (Array.length buf) len)
                     in
                     Audio.S16LE.of_audio buf ofs sbuf 0 len;
                     Pcm.writei pcm sbuf 0 len));
              Pcm.set_channels dev params channels;
              alsa_rate <-
                Pcm.set_rate_near dev params samples_per_second Dir_eq;

              (* Size in frames, must be set after the samplerate.
               * This setting is critical as a too small bufsize will easily result in
               * underruns when the thread isn't fast enough.
               * TODO make it customizable *)
              let bufsize =
                if alsa_buffer > 0 then
                  Pcm.set_buffer_size_near dev params alsa_buffer
                else Pcm.get_buffer_size_max params
              in
              if periods > 0 then Pcm.set_periods dev params periods Dir_eq;
              (bufsize, fst (Pcm.get_periods_max params))
            in
            self#log#important
              "channels=%d, samplerate=%dHz, buffer size=%dB, frame size=%dB, \
               periods=%d"
              channels alsa_rate bufsize
              (Pcm.get_frame_size params)
              periods;
            Pcm.set_params dev params;
            device <- Some dev;
            dev

    method close =
      match device with
        | Some d ->
            Pcm.close d;
            device <- None
        | None -> ()

    method push_block data =
      let dev = self#get_device in
      try
        let len = Audio.length data in
        let rec f pos =
          if pos < len then (
            let ret = alsa_write dev data pos (len - pos) in
            f (pos + ret))
        in
        f 0
      with e ->
        let bt = Printexc.get_raw_backtrace () in
        begin
          match e with
            | Buffer_xrun -> self#log#severe "Underrun!"
            | _ -> self#log#severe "Alsa error: %s" (string_of_error e)
        end;
        if e = Buffer_xrun || e = Suspended || e = Interrupted then (
          self#log#severe "Trying to recover.";
          Pcm.recover dev e)
        else Printexc.raise_with_backtrace e bt

    method send_frame buf =
      let buf = AFrame.pcm buf in
      let ratio = float alsa_rate /. float samples_per_second in
      let buf, ofs, len =
        Audio_converter.Samplerate.resample self#samplerate_converter ratio buf
          0 (Audio.length buf)
      in
      let f data = Audio.blit buf ofs data 0 len in
      ioring#put_block f

    method! reset =
      self#close;
      ignore self#get_device
  end

(* It is registered in Alsa_io. *)
