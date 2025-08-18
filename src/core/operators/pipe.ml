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

open Mm
open Source

type next_stop =
  [ `Metadata of Frame.metadata
  | `Break_and_metadata of Frame.metadata
  | `Break
  | `Sleep
  | `Nothing ]

type chunk = {
  sbuf : Bytes.t;
  next : next_stop;
  mutable ofs : int;
  mutable len : int;
}

class pipe ~replay_delay ~data_len ~process ~bufferize ~max ~restart
  ~restart_on_error source_val =
  let bufferize = Frame.main_of_seconds bufferize in
  let abg_max_len = Frame.audio_of_seconds max in
  let replay_delay = Frame.audio_of_seconds replay_delay in
  let resampler = Decoder_utils.samplerate_converter () in
  let len = match data_len with x when x < 0 -> None | l -> Some l in
  let mutex = Mutex.create () in
  let replay_pending = ref [] in
  let next_stop = ref `Nothing in
  let header_read = ref false in
  let bytes = Bytes.create Utils.pagesize in
  let source = Lang.to_source source_val in
  object (self)
    inherit Simple_encoder.base ~bufferize ~name:"pipe" source
    val mutable samplesize = 16
    val mutable samplerate = Frame.audio_of_seconds 1.

    (* Filled in by wake_up. *)
    val mutable converter = fun _ _ _ -> assert false

    method private header =
      Bytes.unsafe_of_string
        (Wav_aiff.wav_header ~channels:self#audio_channels
           ~sample_rate:samplerate ?len ~sample_size:16 ())

    method private on_start push =
      Process_handler.really_write self#header push;
      `Continue

    method private on_stdout pull =
      if not !header_read then (
        let wav = Wav_aiff.read_header Wav_aiff.callback_ops pull in
        header_read := true;
        Mutex_utils.mutexify mutex
          (fun () ->
            if Wav_aiff.channels wav <> self#audio_channels then
              failwith "Invalid channels from pipe process!";
            samplesize <- Wav_aiff.sample_size wav;
            samplerate <- Wav_aiff.sample_rate wav;
            converter <-
              Decoder_utils.from_iff ~format:`Wav ~channels:self#audio_channels
                ~samplesize)
          ();
        `Reschedule `Non_blocking)
      else (
        let len = pull bytes 0 Utils.pagesize in
        let data = converter bytes 0 len in
        let data, ofs, len = resampler ~samplerate data 0 (Audio.length data) in
        let buffered = Generator.length self#buffer in
        let duration = Frame.main_of_audio len in
        let offset = Frame.main_of_audio ofs in
        Generator.put self#buffer Frame.Fields.audio
          (Content.Audio.lift_data ~offset ~length:duration data);
        let to_replay =
          Mutex_utils.mutexify mutex
            (fun () ->
              let pending = !replay_pending in
              let to_replay, pending =
                List.fold_left
                  (fun ((pos, b), cur) (pos', b') ->
                    if pos' + len > replay_delay then (
                      if pos > 0 then
                        self#log#info
                          "Cannot replay multiple element at once.. Picking up \
                           the most recent";
                      if pos > 0 && pos < pos' then ((pos, b), cur)
                      else ((pos', b'), cur))
                    else ((pos, b), (pos' + len, b') :: cur))
                  ((-1, `Nothing), [])
                  pending
              in
              replay_pending := pending;
              to_replay)
            ()
        in
        begin
          match to_replay with
            | -1, _ -> ()
            | _, `Break_and_metadata m ->
                Generator.add_metadata self#buffer m;
                Generator.add_track_mark self#buffer
            | _, `Metadata m -> Generator.add_metadata self#buffer m
            | _, `Break -> Generator.add_track_mark self#buffer
            | _ -> ()
        end;
        if abg_max_len < buffered + len then
          `Delay (Frame.seconds_of_audio (buffered + len - abg_max_len))
        else `Continue)

    val mutable handler = None
    val to_write = Queue.create ()

    method private get_handler =
      match handler with Some h -> h | None -> raise Process_handler.Finished

    method encode =
      function
      | `Flush -> ()
      | `Frame frame ->
          let buf = AFrame.pcm frame in
          let blen = Audio.length buf in
          let slen_of_len len = 2 * len * Array.length buf in
          let slen = slen_of_len blen in
          let sbuf = Bytes.create slen in
          Audio.S16LE.of_audio buf 0 sbuf 0 blen;
          let metadata =
            List.sort
              (fun (pos, _) (pos', _) -> compare pos pos')
              (Frame.get_all_metadata frame)
          in
          let track_mark =
            match Frame.track_marks frame with p :: _ -> Some p | [] -> None
          in
          let ofs =
            List.fold_left
              (fun ofs (pos, m) ->
                let pos = slen_of_len pos in
                let len = pos - ofs in
                let next =
                  if track_mark = Some pos then `Break_and_metadata m
                  else `Metadata m
                in
                Queue.push { sbuf; next; ofs; len } to_write;
                pos)
              0 metadata
          in
          if ofs < slen then (
            let len = slen - ofs in
            let next = if track_mark <> None then `Break else `Nothing in
            Queue.push { sbuf; next; ofs; len } to_write)

    method private on_stdin pusher =
      try
        let ({ sbuf; next; ofs; len } as chunk) = Queue.peek to_write in
        (* Select documentation: large write may still block.. *)
        let wlen = min Utils.pagesize len in
        let ret = pusher sbuf ofs wlen in
        if ret = len then (
          let action =
            if next <> `Nothing && replay_delay >= 0 then (
              Mutex_utils.mutexify mutex
                (fun () -> replay_pending := (0, next) :: !replay_pending)
                ();
              `Continue)
            else (
              Mutex_utils.mutexify mutex (fun () -> next_stop := next) ();
              if next <> `Nothing then `Stop else `Continue)
          in
          ignore (Queue.take to_write);
          action)
        else (
          chunk.ofs <- ofs + ret;
          chunk.len <- len - ret;
          `Continue)
      with Queue.Empty -> `Continue

    method private on_stderr reader =
      let len = reader bytes 0 Utils.pagesize in
      self#log#important "Error: %s"
        (Bytes.unsafe_to_string (Bytes.sub bytes 0 len));
      `Continue

    method private on_stop =
      Mutex_utils.mutexify mutex (fun e ->
          let ret = !next_stop in
          next_stop := `Nothing;
          header_read := false;
          let should_restart =
            match e with
              | `Status s when s <> Unix.WEXITED 0 -> restart_on_error
              | `Exception _ -> restart_on_error
              | _ -> true
          in
          match (should_restart, ret) with
            | false, _ -> false
            | _, `Sleep -> false
            | _, `Break_and_metadata m ->
                Generator.add_metadata self#buffer m;
                Generator.add_track_mark self#buffer;
                true
            | _, `Metadata m ->
                Generator.add_metadata self#buffer m;
                true
            | _, `Break ->
                Generator.add_track_mark self#buffer;
                true
            | _, `Nothing -> restart)

    initializer
      self#on_wake_up (fun () ->
          source#wake_up (self :> Clock.source);
          converter <-
            Decoder_utils.from_iff ~format:`Wav ~channels:self#audio_channels
              ~samplesize;

          handler <-
            Some
              (Process_handler.run ~on_stop:self#on_stop ~on_start:self#on_start
                 ~on_stdout:self#on_stdout ~on_stdin:self#on_stdin
                 ~priority:`Blocking ~on_stderr:self#on_stderr
                 ~log:(self#log#info "%s") process));
      self#on_sleep (fun () ->
          source#sleep (self :> Clock.source);
          Mutex_utils.mutexify mutex
            (fun () ->
              try
                next_stop := `Sleep;
                replay_pending := [];
                Process_handler.stop self#get_handler;
                handler <- None
              with Process_handler.Finished -> ())
            ())
  end

let _ =
  let return_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator "pipe"
    [
      ("process", Lang.string_t, None, Some "Process used to pipe data to.");
      ( "replay_delay",
        Lang.nullable_t Lang.float_t,
        Some Lang.null,
        Some
          "Replay track marks and metadata from the input source on the output \
           after a given delay. If `null` (default) close and flush the \
           process on each track and metadata to get an exact timing. This \
           parameter is typically used when integrating with `stereotool`." );
      ( "data_length",
        Lang.nullable_t Lang.int_t,
        Some Lang.null,
        Some
          "Length passed in the WAV data chunk. Data is streamed so no the \
           consuming program should process it as it comes. Some program \
           operate better when this value is set to `0`, some other when it is \
           set to the maximum length allowed by the WAV specs. Use any \
           negative value to set to maximum length." );
      ( "buffer",
        Lang.float_t,
        Some (Lang.float 1.),
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
        Some (Lang.bool true),
        Some "Restart process when exited with error." );
      ("", Lang.source_t return_t, None, None);
    ]
    ~return_t ~category:`Audio
    ~descr:"Process audio signal through a given process stdin/stdout."
    (fun p ->
      let f v = List.assoc v p in
      let ( process,
            replay_delay,
            data_len,
            bufferize,
            max,
            restart,
            restart_on_error,
            src ) =
        ( Lang.to_string (f "process"),
          Lang.to_option (f "replay_delay"),
          Lang.to_option (f "data_length"),
          Lang.to_float (f "buffer"),
          Lang.to_float (f "max"),
          Lang.to_bool (f "restart"),
          Lang.to_bool (f "restart_on_error"),
          f "" )
      in
      let replay_delay =
        match replay_delay with None -> -1. | Some v -> Lang.to_float v
      in
      let data_len =
        match data_len with None -> -1 | Some v -> Lang.to_int v
      in
      (new pipe
         ~replay_delay ~data_len ~bufferize ~max ~restart ~restart_on_error
         ~process src
        :> source))
