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

(** External encoder *)

open External_encoder_format

let encoder id ext =
  let log = Log.make [id] in
  let is_metadata_restart = ref false in
  let is_stop = ref false in
  let buf = Strings.Mutable.empty () in
  let bytes = Bytes.create Utils.pagesize in
  let mutex = Mutex.create () in
  let condition = Condition.create () in
  let restart_decision =
    Tutils.mutexify mutex (fun () ->
        let decision =
          match (!is_metadata_restart, !is_stop) with
            | _, true -> false
            | true, false -> true
            | false, false -> ext.restart_on_crash
        in
        is_metadata_restart := false;
        decision)
  in
  let header =
    if ext.video then
      Avi.header ~channels:ext.channels
        ~samplerate:(Lazy.force ext.samplerate)
        ()
    else if ext.header then
      Wav_aiff.wav_header ~channels:ext.channels
        ~sample_rate:(Lazy.force ext.samplerate)
        ~sample_size:16 ()
    else ""
  in
  let on_stderr puller =
    let len = puller bytes 0 Utils.pagesize in
    log#debug "stderr: %s" (Bytes.unsafe_to_string (Bytes.sub bytes 0 len));
    `Continue
  in
  let on_start pusher =
    Process_handler.really_write (Bytes.of_string header) pusher;
    `Continue
  in
  let on_stop = function
    | `Status s ->
        begin
          match s with
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
  let log = log#important "%s" in
  let on_stdout =
    Tutils.mutexify mutex (fun puller ->
        begin
          let len = puller bytes 0 Utils.pagesize in
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
  let insert_metadata =
    Tutils.mutexify mutex (fun _ ->
        if ext.restart = Metadata then (
          is_metadata_restart := true;
          Process_handler.stop process ))
  in
  let converter = Audio_converter.Samplerate.create ext.channels in
  let ratio =
    float (Lazy.force ext.samplerate) /. float (Frame.audio_of_seconds 1.)
  in
  let encode frame start len =
    let channels = ext.channels in
    let sbuf =
      if ext.video then
        Avi_encoder.encode_frame ~channels
          ~samplerate:(Lazy.force ext.samplerate)
          ~converter frame start len
      else (
        let start = Frame.audio_of_master start in
        let b = AFrame.content_of_type ~channels frame start in
        let len = Frame.audio_of_master len in
        (* Resample if needed. *)
        let b, start, len =
          if ratio = 1. then (b, start, len)
          else (
            let b =
              Audio_converter.Samplerate.resample converter ratio
                (Audio.sub b start len)
            in
            (b, 0, Audio.length b) )
        in
        let slen = 2 * len * Array.length b in
        let sbuf = Bytes.create slen in
        Audio.S16LE.of_audio (Audio.sub b start len) sbuf 0;
        Strings.unsafe_of_bytes sbuf )
    in
    Tutils.mutexify mutex
      (fun () ->
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
          ())
      ();
    Strings.Mutable.flush buf
  in
  let stop =
    Tutils.mutexify mutex (fun () ->
        is_stop := true;
        Process_handler.stop process;
        Condition.wait condition mutex;
        Strings.Mutable.flush buf)
  in
  {
    Encoder.insert_metadata;
    (* External encoders do not support 
     * headers for now. They will probably
     * never do.. *)
    header = Strings.empty;
    encode;
    stop;
  }

let () =
  Encoder.plug#register "EXTERNAL" (function
    | Encoder.External m -> Some (fun s _ -> encoder s m)
    | _ -> None)
