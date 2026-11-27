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

open Mm

(** External encoder *)

open External_encoder_format

let encoder id ext =
  let log = Log.make [id] in
  let is_metadata_restart = ref false in
  let is_stop = ref false in
  let buf = Strings.Mutable.empty () in
  let bytes = Bytes.create Utils.buflen in
  let mutex = Mutex.create () in
  let condition = Condition.create () in
  let restart_decision () =
    Mutex.protect mutex (fun () ->
        let decision =
          match (!is_metadata_restart, !is_stop) with
            | _, true -> -1.
            | true, false -> 0.
            | false, false -> if ext.restart_on_crash then 0. else -1.
        in
        is_metadata_restart := false;
        decision)
  in
  let header =
    if ext.video <> None then (
      let width, height = Option.get ext.video in
      let width = Deferred.force width in
      let height = Deferred.force height in
      Avi.header ~width ~height ~channels:ext.channels
        ~samplerate:(Deferred.force ext.samplerate)
        ())
    else if ext.header then
      Wav_aiff.wav_header ~channels:ext.channels
        ~sample_rate:(Deferred.force ext.samplerate)
        ~sample_size:16 ()
    else ""
  in
  let on_stderr puller =
    let len = puller bytes 0 Utils.buflen in
    log#debug "stderr: %s" (Bytes.unsafe_to_string (Bytes.sub bytes 0 len));
    `Continue
  in
  let on_start pusher =
    Process_handler.really_write (Bytes.of_string header) pusher;
    `Continue
  in
  let on_stop = function
    | `Status s ->
        begin match s with
          | Unix.WEXITED 0 -> ()
          | Unix.WEXITED c -> log#important "Process exited with code %d" c
          | Unix.WSIGNALED s ->
              log#important "Process was killed by signal %d" s
          | Unix.WSTOPPED s ->
              log#important "Process was stopped by signal %d" s
        end;
        restart_decision ()
    | `Exception e ->
        log#important "Error: %s" (Printexc.to_string e);
        restart_decision ()
  in
  (* Signal end of process instead of always waiting
     for zero read. See: https://lkml.indiana.edu/hypermail/linux/kernel/0106.0/0768.html *)
  let on_stop v =
    Condition.signal condition;
    on_stop v
  in
  let log s = log#important "%s" s in
  let on_stdout puller =
    Mutex.protect mutex (fun () ->
        begin
          let len = puller bytes 0 Utils.buflen in
          match len with
            | 0 when !is_stop -> Condition.signal condition
            | _ -> Strings.Mutable.add_subbytes buf bytes 0 len
        end;
        `Continue)
  in
  let process =
    Process_handler.run ~on_start ~on_stop ~on_stdout ~on_stderr ~log
      ext.process
  in
  let encode_metadata _ =
    Mutex.protect mutex (fun () ->
        if ext.restart = Metadata then (
          is_metadata_restart := true;
          Process_handler.stop process))
  in
  let converter = Audio_converter.Samplerate.create ext.channels in
  let ratio =
    float (Deferred.force ext.samplerate) /. float (Frame.audio_of_seconds 1.)
  in
  let encode frame =
    let channels = ext.channels in
    let samplerate = Deferred.force ext.samplerate in
    let sbuf =
      if ext.video <> None then (
        let width, height = Option.get ext.video in
        let width = Deferred.force width in
        let height = Deferred.force height in
        let video, audio =
          Avi_encoder.encode_frame ~channels ~samplerate ~converter ~width
            ~height frame
        in
        Strings.append video audio)
      else (
        let b = AFrame.pcm frame in
        let len = AFrame.position frame in
        (* Resample if needed. *)
        let b, start, len =
          Audio_converter.Samplerate.resample converter ratio b 0 len
        in
        let slen = 2 * len * Array.length b in
        let sbuf = Bytes.create slen in
        Audio.S16LE.of_audio b start sbuf 0 len;
        Strings.unsafe_of_bytes sbuf)
    in
    Mutex.protect mutex (fun () ->
        try
          Process_handler.on_stdin process (fun push ->
              Strings.iter
                (fun s offset length ->
                  Process_handler.really_write ~offset ~length
                    (Bytes.unsafe_of_string s) push)
                sbuf)
        with
        | Process_handler.Finished
        when ext.restart_on_crash || !is_metadata_restart
        ->
          ());
    Strings.Mutable.flush buf
  in
  (* The process gets what the rate converter still holds, audio only. *)
  let write_drained () =
    if ext.video = None then (
      let tail = Audio_converter.Samplerate.drain converter in
      let len = Audio.length tail in
      if 0 < len then (
        let sbuf = Bytes.create (2 * len * Array.length tail) in
        Audio.S16LE.of_audio tail 0 sbuf 0 len;
        try
          Process_handler.on_stdin process (fun push ->
              Process_handler.really_write ~offset:0 ~length:(Bytes.length sbuf)
                sbuf push)
        with Process_handler.Finished -> ()))
  in
  let stop () =
    write_drained ();
    Mutex.protect mutex (fun () ->
        is_stop := true;
        Process_handler.stop process;
        Condition.wait condition mutex;
        Strings.Mutable.flush buf)
  in
  {
    Encoder.encode_metadata;
    header = (fun () -> Strings.empty);
    encode;
    stop;
    bitrate = (fun () -> None);
    access_points = None;
    restart_offset = (fun () -> None);
  }

let () =
  Plug.register Encoder.plug "external" ~doc:"Encode using external programs."
    (Encoder.of_encoders (function
      | Encoder.External m -> Some (fun ~pos:_ s _ -> encoder s m)
      | _ -> None))
