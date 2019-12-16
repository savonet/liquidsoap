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

open Extralib
module Generator = Generator.From_audio_video_plus
module Generated = Generated.From_audio_video_plus

(* {1 External Input handling} *)

exception Finished of string * bool

class external_input ~name ~kind ~restart ~bufferize ~restart_on_error ~max
  ~converter ?read_header command =
  let abg_max_len = Frame.audio_of_seconds max in
  (* We need a temporary log until the source has an id *)
  let log_ref = ref (fun _ -> ()) in
  let log x = !log_ref x in
  let abg = Generator.create ~log ~kind `Audio in
  let buflen = Utils.pagesize in
  let buf = Bytes.create buflen in
  let on_data reader =
    let ret = reader buf 0 buflen in
    let data = converter (Bytes.sub_string buf 0 ret) in
    let len = Audio.length data in
    let buffered = Generator.length abg in
    Generator.put_audio abg data 0 len;
    if abg_max_len < buffered + len then
      `Delay (Frame.seconds_of_audio (buffered + len - (3 * abg_max_len / 4)))
    else `Continue
  in
  object (self)
    inherit
      External_input.base
        ~name ~kind ?read_header ~restart ~restart_on_error ~on_data command as base

    inherit Generated.source abg ~empty_on_abort:false ~bufferize

    method wake_up x =
      (* Now we can create the log function *)
      log_ref := self#log#important "%s";
      base#wake_up x
  end

let proto =
  [
    ( "buffer",
      Lang.float_t,
      Some (Lang.float 2.),
      Some "Duration of the pre-buffered data." );
    ( "max",
      Lang.float_t,
      Some (Lang.float 10.),
      Some "Maximum duration of the buffered data." );
    ( "restart",
      Lang.bool_t,
      Some (Lang.bool true),
      Some "Restart process when exited." );
    ( "restart_on_error",
      Lang.bool_t,
      Some (Lang.bool false),
      Some "Restart process when exited with error." );
    ("", Lang.string_t, None, Some "Command to execute.");
  ]

let () =
  Lang.add_operator "input.external.rawaudio" ~category:Lang.Input
    ~descr:"Stream raw PCM data from an external application."
    ( proto
    @ [
        ("channels", Lang.int_t, Some (Lang.int 2), Some "Number of channels.");
        ("samplerate", Lang.int_t, Some (Lang.int 44100), Some "Samplerate.");
      ] )
    ~kind:Lang.audio_any
    (fun p kind ->
      let command = Lang.to_string (List.assoc "" p) in
      let bufferize = Lang.to_float (List.assoc "buffer" p) in
      let channels = Lang.to_int (List.assoc "channels" p) in
      if not (Frame.mul_eq_int kind.Frame.audio channels) then
        raise
          (Lang_errors.Invalid_value
             ( List.assoc "channels" p,
               "Incompatible number of channels, please use a conversion \
                operator." ));
      let audio_src_rate = float (Lang.to_int (List.assoc "samplerate" p)) in
      let converter =
        Rutils.create_from_iff ~format:`Wav ~channels ~samplesize:16
          ~audio_src_rate
      in
      let restart = Lang.to_bool (List.assoc "restart" p) in
      let restart_on_error = Lang.to_bool (List.assoc "restart_on_error" p) in
      let max = Lang.to_float (List.assoc "max" p) in
      ( new external_input
          ~kind ~restart ~bufferize ~restart_on_error ~max
          ~name:"input.external.rawaudio" ~converter command
        :> Source.source ))

let () =
  Lang.add_operator "input.external.wav" ~category:Lang.Input
    ~descr:"Stream WAV data from an external application." proto
    ~kind:Lang.audio_any (fun p kind ->
      let command = Lang.to_string (List.assoc "" p) in
      let bufferize = Lang.to_float (List.assoc "buffer" p) in
      let converter_ref = ref (fun _ -> assert false) in
      let converter data = !converter_ref data in
      let read_header read =
        let header = Wav_aiff.read_header Wav_aiff.callback_ops read in
        let channels = Wav_aiff.channels header in
        let audio_src_rate = float (Wav_aiff.sample_rate header) in
        let samplesize = Wav_aiff.sample_size header in
        Wav_aiff.close header;
        converter_ref :=
          Rutils.create_from_iff ~format:`Wav ~channels ~samplesize
            ~audio_src_rate;
        `Reschedule Tutils.Non_blocking
      in
      let restart = Lang.to_bool (List.assoc "restart" p) in
      let restart_on_error = Lang.to_bool (List.assoc "restart_on_error" p) in
      let max = Lang.to_float (List.assoc "max" p) in
      ( new external_input
          ~kind ~restart ~bufferize ~read_header ~restart_on_error ~max
          ~name:"input.external.wav" ~converter command
        :> Source.source ))
