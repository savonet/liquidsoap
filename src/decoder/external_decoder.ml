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

(** Decode files using an external decoder. *)

(** First, an external decoder that receives
  * on its stdin. *)

let log = Log.make ["decoder"; "external"]

let on_stderr =
  let buf = Bytes.create Utils.pagesize in
  fun puller ->
    let len = puller buf 0 Utils.pagesize in
    log#debug "stderr: %s" (Bytes.unsafe_to_string (Bytes.sub buf 0 len));
    `Continue

(** This function is used to wrap around the "real" input.
  * It pipes its data to the external process and read
  * the available output. *)
let external_input process input =
  let buflen = Utils.pagesize in
  let buf = Bytes.create buflen in
  let on_stdin pusher =
    let read = input.Decoder.read buf 0 buflen in
    if read = 0 then `Stop
    else begin
      Process_handler.really_write (Bytes.sub buf 0 read) pusher;
      `Continue
    end
  in
  let log = log#important "%s" in
  (* reading from input is blocking.. *)
  let priority = Tutils.Blocking in
  let process =
    Process_handler.run ~priority ~on_stdin ~on_stderr ~log process
  in
  let read buf ofs len =
    try Process_handler.on_stdout process (fun reader -> reader buf ofs len)
    with Process_handler.Finished -> 0
  in
  ( { Decoder.read; tell = None; length = None; lseek = None },
    fun () ->
      try Process_handler.kill process with Process_handler.Finished -> () )

let duration process =
  let pull = Unix.open_process_in process in
  let w = Wav_aiff.in_chan_read_header pull in
  let ret = Wav_aiff.duration w in
  ignore (Unix.close_process_in pull);
  ret

module Generator = Generator.From_audio_video
module Buffered = Decoder.Buffered (Generator)

(** A function to wrap around the Wav_aiff_decoder *)
let create process kind filename =
  let close = ref (fun () -> ()) in
  let create input =
    let input, actual_close = external_input process input in
    close := actual_close;
    Wav_aiff_decoder.D.create ?header:None input
  in
  let generator = Generator.create `Audio in
  let dec = Buffered.file_decoder filename kind create generator in
  {
    dec with
    Decoder.close =
      (fun () -> Tutils.finalize ~k:(fun () -> dec.Decoder.close ()) !close);
  }

let create_stream process input =
  let input, close = external_input process input in
  (* Put this here so that ret is not in its closure.. *)
  let close _ = close () in
  let ret = Wav_aiff_decoder.D_stream.create input in
  Gc.finalise close ret;
  ret

let test_kind f filename =
  (* 0 = file rejected,
   * n<0 = file accepted, unknown number of audio channels,
   * n>0 = file accepted, known number of channels. *)
  let ret = f filename in
  if ret = 0 then None
  else
    Some
      {
        Frame.video = Frame.Zero;
        midi = Frame.Zero;
        audio =
          (if ret < 0 then Frame.Succ Frame.Variable else Frame.mul_of_int ret);
      }

let register_stdin name sdoc mimes test process =
  Decoder.file_decoders#register name ~sdoc (fun ~metadata:_ filename kind ->
      match test_kind test filename with
        | None -> None
        | Some out_kind ->
            (* Check that kind is more permissive than out_kind and
             * declare that our decoding function will respect out_kind. *)
            if Frame.kind_sub_kind out_kind kind then
              Some (fun () -> create process out_kind filename)
            else None);
  let duration filename =
    let process = Printf.sprintf "cat %s | %s" (Utils.quote filename) process in
    duration process
  in
  Request.dresolvers#register name duration;
  if mimes <> [] then
    Decoder.stream_decoders#register name
      ~sdoc:
        (Printf.sprintf
           "Use %s to decode any stream with an appropriate MIME type." name)
      (fun mime kind ->
        let ( <: ) a b = Frame.mul_sub_mul a b in
        if
          List.mem mime mimes
          (* Check that it is okay to have zero video and midi,
           * and at least one audio channel. *)
          && Frame.Zero <: kind.Frame.video
          && Frame.Zero <: kind.Frame.midi
          && kind.Frame.audio <> Frame.Zero
        then
          (* In fact we can't be sure that we'll satisfy the content
           * kind, because the stream might be mono or stereo.
           * For now, we let this problem result in an error at
           * decoding-time. Failing early would only be an advantage
           * if there was possibly another plugin for decoding
           * correctly the stream (e.g. by performing conversions). *)
          Some (create_stream process)
        else None)

(** Now an external decoder that directly operates
  * on the file. The remaining time in this case
  * can only be approximative. It is -1 while
  * the file is being decoded and the length
  * of the buffer when the external decoder
  * has exited. *)

let log = Log.make ["decoder"; "external"; "oblivious"]

let external_input_oblivious process filename prebuf =
  let command = process filename in
  let process =
    Process_handler.run ~on_stderr ~log:(log#important "%s") command
  in
  let read buf ofs len =
    try Process_handler.on_stdout process (fun reader -> reader buf ofs len)
    with Process_handler.Finished -> 0
  in
  let close () =
    try Process_handler.kill process with Process_handler.Finished -> ()
  in
  let input = { Decoder.read; tell = None; length = None; lseek = None } in
  let gen = Generator.create `Audio in
  let prebuf = Frame.master_of_seconds prebuf in
  let decoder = Wav_aiff_decoder.D.create input in
  let fill frame =
    begin
      try
        while
          Generator.length gen < prebuf && not (Process_handler.stopped process)
        do
          decoder.Decoder.decode gen
        done
      with e ->
        log#info "Decoding %s ended: %s." command (Printexc.to_string e);
        close ()
    end;
    Generator.fill gen frame;
    (* We return -1 while the process is not yet
     * finished. *)
    if Process_handler.stopped process then Generator.length gen else -1
  in
  { Decoder.fill; fseek = decoder.Decoder.seek; close }

let register_oblivious name sdoc test process prebuf =
  Decoder.file_decoders#register name ~sdoc (fun ~metadata:_ filename kind ->
      match test_kind test filename with
        | None -> None
        | Some out_kind ->
            (* Check that kind is more permissive than out_kind and
             * declare that our decoding function will respect out_kind. *)
            if Frame.kind_sub_kind out_kind kind then
              Some (fun () -> external_input_oblivious process filename prebuf)
            else None);
  let duration filename = duration (process filename) in
  Request.dresolvers#register name duration
