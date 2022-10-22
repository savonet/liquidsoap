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

(* Alsa calls are blocking AND they do not respond in a regular fashion.
 * Basically, it waits two buffer lengths, then give you one buffer,
 * then waits epsilon and gives you the second one.
 * To make it smooth and avoid the root scheduling to be affected by alsa
 * problems (and vice versa), we thread and bufferize a LITTLE bit. *)

open Alsa

class mic ~clock_safe ~fallible ~on_start ~on_stop ~start device =
  let buffer_length = AFrame.size () in
  let alsa_device = device in
  let nb_blocks = Alsa_settings.conf_buffer_length#get in
  object (self)
    inherit
      Start_stop.active_source
        ~name:"input.alsa" ~fallible ~on_start ~on_stop ~autostart:start
          ~clock_safe ~get_clock:Alsa_settings.get_clock () as active_source

    inherit Source.no_seek
    inherit [Content.Audio.data] IoRing.input ~nb_blocks as ioring
    val mutable initialized = false
    method self_sync = (`Static, true)

    method private wake_up l =
      active_source#wake_up l;
      let blank () = Audio.make self#audio_channels buffer_length 0. in
      ioring#init blank

    method private sleep =
      active_source#sleep;
      ioring#sleep

    method abort_track = ()
    method remaining = -1

    (* val mutable alsa_fmt = *)
    val mutable sample_freq = Lazy.force Frame.audio_rate

    val mutable read_fun =
      fun pcm (buf : Content.Audio.data) ofs len ->
        Pcm.readn_float pcm buf ofs len

    val mutable device = None

    method close =
      match device with
        | Some d ->
            Pcm.close d;
            device <- None
        | None -> ()

    method get_device =
      match device with
        | Some d -> d
        | None ->
            self#log#info "Using ALSA %s." (Alsa.get_version ());
            let dev = Pcm.open_pcm alsa_device [Pcm.Capture] [] in
            let params = Pcm.get_params dev in
            begin
              try
                Pcm.set_access dev params Pcm.Access_rw_noninterleaved;
                Pcm.set_format dev params Pcm.Format_float
              with _ ->
                (* If we can't get floats we fallback on interleaved s16le *)
                self#log#severe "Falling back on interleaved S16LE";
                Pcm.set_access dev params Pcm.Access_rw_interleaved;
                Pcm.set_format dev params Pcm.Format_s16_le;
                read_fun <-
                  (fun pcm buf ofs len ->
                    let sbuf = Bytes.make (2 * 2 * len) (Char.chr 0) in
                    let r = Pcm.readi pcm sbuf 0 len in
                    Audio.S16LE.to_audio
                      (Bytes.unsafe_to_string sbuf)
                      0 buf ofs r;
                    r)
            end;
            sample_freq <- Pcm.set_rate_near dev params sample_freq Dir_eq;

            (* TODO: resample *)
            if sample_freq <> Lazy.force Frame.audio_rate then
              self#log#important
                "Got a sampling frequency of %d instead of %d (TODO: should be \
                 resampled in the future)."
                sample_freq
                (Lazy.force Frame.audio_rate);
            Pcm.set_channels dev params self#audio_channels;
            Pcm.set_params dev params;
            Pcm.prepare dev;
            device <- Some dev;
            dev

    method pull_block block =
      let dev = self#get_device in
      try
        let pos = ref 0 in
        while !pos < buffer_length do
          let len = buffer_length - !pos in
          let ret = read_fun dev block !pos len in
          assert (ret <= len);
          pos := !pos + ret
        done
      with e ->
        begin
          match e with
            | Buffer_xrun -> self#log#important "Overrun!"
            | _ -> self#log#severe "Alsa error: %s" (string_of_error e)
        end;
        if e = Buffer_xrun || e = Suspended || e = Interrupted then (
          self#log#severe "Trying to recover..";
          Pcm.recover dev e)
        else raise e

    method get_frame buf =
      assert (0 = AFrame.position buf);
      let buffer = ioring#get_block in
      let fbuf = AFrame.pcm buf in
      Audio.blit buffer 0 fbuf 0 buffer_length;
      AFrame.add_break buf buffer_length

    method reset = ()
  end

(* This source is registered in Alsa_io. *)
