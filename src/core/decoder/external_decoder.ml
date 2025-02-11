(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

(** First, an external decoder that receives on its stdin. *)

let log = Log.make ["decoder"; "external"]

let on_stderr =
  let buf = Bytes.create Utils.pagesize in
  fun puller ->
    let len = puller buf 0 Utils.pagesize in
    log#debug "stderr: %s" (Bytes.unsafe_to_string (Bytes.sub buf 0 len));
    `Continue

(** This function is used to wrap around the "real" input. It pipes its data to
    the external process and read the available output. *)
let external_input process input =
  let buflen = Utils.pagesize in
  let buf = Bytes.create buflen in
  let on_stdin pusher =
    let read = input.Decoder.read buf 0 buflen in
    if read = 0 then `Stop
    else (
      Process_handler.really_write (Bytes.sub buf 0 read) pusher;
      `Continue)
  in
  let log s = log#important "%s" s in
  (* reading from input is blocking.. *)
  let priority = `Blocking in
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

(** A function to wrap around the Wav_aiff_decoder *)
let create process ctype filename =
  let close = ref (fun () -> ()) in
  let create input =
    let input, actual_close = external_input process input in
    close := actual_close;
    Wav_aiff_decoder.create ?header:None input
  in
  let dec = Decoder.opaque_file_decoder ~filename ~ctype create in
  {
    dec with
    Decoder.fclose =
      (fun () -> Fun.protect ~finally:(fun () -> dec.Decoder.fclose ()) !close);
  }

let create_stream process input =
  let input, close = external_input process input in
  (* Put this here so that ret is not in its closure.. *)
  let close _ = close () in
  let ret = Wav_aiff_decoder.create input in
  Gc.finalise close ret;
  ret

let audio_n = Frame_base.format_of_channels ~pcm_kind:Content.Audio.kind

let test_ctype f filename =
  (* 0 = file rejected,
     n<0 = file accepted, unknown number of audio channels,
     n>0 = file accepted, known number of channels. *)
  let ret = f filename in
  if ret = 0 then None
  else
    Some
      (Frame.Fields.make
         ~audio:
           (if ret < 0 then audio_n (Lazy.force Frame.audio_channels)
            else audio_n ret)
         ())

let register_stdin ~name ~doc ~priority ~mimes ~file_extensions ~test process =
  Plug.register Decoder.decoders name ~doc
    {
      Decoder.priority = (fun () -> priority);
      file_extensions = (fun () -> file_extensions);
      mime_types = (fun () -> mimes);
      file_type =
        (fun ~metadata:_ ~ctype:_ filename -> test_ctype test filename);
      file_decoder =
        Some (fun ~metadata:_ ~ctype filename -> create process ctype filename);
      stream_decoder = Some (fun ~ctype:_ _ -> create_stream process);
    };

  let dresolver ~metadata:_ filename =
    let process =
      Printf.sprintf "cat %s | %s" (Filename.quote filename) process
    in
    duration process
  in
  Plug.register Request.dresolvers name ~doc
    {
      dpriority = (fun () -> priority);
      file_extensions = (fun () -> Option.value ~default:[] file_extensions);
      dresolver;
    }

(** Now an external decoder that directly operates on the file. The remaining
    time in this case can only be approximative. It is -1 while the file is
    being decoded and the length of the buffer when the external decoder has
    exited. *)

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
  let fclose () =
    try Process_handler.kill process with Process_handler.Finished -> ()
  in
  let input = { Decoder.read; tell = None; length = None; lseek = None } in
  (* TODO: is this really what we want for audio channels? *)
  let ctype =
    Frame.Fields.make ~audio:(audio_n (Lazy.force Frame.audio_channels)) ()
  in
  let gen = Generator.create ~log ctype in
  let buffer = Decoder.mk_buffer ~ctype gen in
  let prebuf = Frame.main_of_seconds prebuf in
  let decoder = Wav_aiff_decoder.create input in
  let fread len =
    begin
      try
        while
          Generator.length gen < max prebuf len
          && not (Process_handler.stopped process)
        do
          decoder.Decoder.decode buffer
        done
      with e ->
        log#info "Decoding %s ended: %s." command (Printexc.to_string e);
        fclose ()
    end;
    Generator.slice gen len
  in
  let remaining () =
    (* We return -1 while the process is not yet
     * finished. *)
    if Process_handler.stopped process then Generator.length gen else -1
  in
  { Decoder.fread; remaining; fseek = decoder.Decoder.seek; fclose }

let register_oblivious ~name ~doc ~priority ~mimes ~file_extensions ~test
    ~process prebuf =
  Plug.register Decoder.decoders name ~doc
    {
      Decoder.priority = (fun () -> priority);
      file_extensions = (fun () -> file_extensions);
      mime_types = (fun () -> mimes);
      file_type =
        (fun ~metadata:_ ~ctype:_ filename -> test_ctype test filename);
      file_decoder =
        Some
          (fun ~metadata:_ ~ctype:_ filename ->
            external_input_oblivious process filename prebuf);
      stream_decoder = None;
    };

  let dresolver ~metadata:_ filename = duration (process filename) in
  Plug.register Request.dresolvers name ~doc
    {
      dpriority = (fun () -> priority);
      file_extensions = (fun () -> Option.value ~default:[] file_extensions);
      dresolver;
    }
