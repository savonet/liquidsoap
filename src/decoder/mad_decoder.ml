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

(** Decode mpeg audio files using libmad. *)

let log = Log.make ["decoder"; "mad"]

let init input =
  let index = Hashtbl.create 10 in
  let time_offset = ref 0 in
  let dec = ref (Mad.openstream input.Decoder.read) in
  (* Skip id3 tags if possible. *)
  begin
    match (input.Decoder.lseek, input.Decoder.tell) with
    | Some seek, Some tell ->
        Mad.skip_id3tags ~read:input.Decoder.read ~seek ~tell
    | _, _ -> ()
  end;
  let get_index time = Hashtbl.find index time in
  let update_index () =
    match input.Decoder.tell with
      | None -> ()
      | Some f ->
          let time = !time_offset + Mad.get_current_time !dec Mad.Seconds in
          if not (Hashtbl.mem index time) then Hashtbl.add index time (f ())
  in
  (* Add an initial index. *)
  update_index ();
  let get_data () =
    let data = Mad.decode_frame_float_ba !dec in
    update_index ();
    data
  in
  let get_time () =
    float !time_offset
    +. (float (Mad.get_current_time !dec Mad.Centiseconds) /. 100.)
  in
  let seek ticks =
    if ticks < 0 && input.Decoder.lseek = None then 0
    else begin
      let time = Frame.seconds_of_master ticks in
      let cur_time = get_time () in
      let seek_time = cur_time +. time in
      let seek_time = if seek_time < 0. then 0. else seek_time in
      if time < 0. then begin
        try
          let seek_time = int_of_float (floor seek_time) in
          let seek_pos = if seek_time > 0 then get_index seek_time else 0 in
          ignore ((Utils.get_some input.Decoder.lseek) seek_pos);
          dec := Mad.openstream input.Decoder.read;
          (* Decode one frame to set the decoder to a good reading position
           * on next read. *)
          ignore (Mad.decode_frame_float !dec);
          (* We have to assume here that new_pos = seek_pos.. *)
          time_offset := seek_time
        with _ -> ()
      end;
      let rec f pos =
        if pos < seek_time then
          if
            try
              Mad.skip_frame !dec;
              true
            with Mad.End_of_stream -> false
          then (
            update_index ();
            f (get_time ()) )
      in
      f (get_time ());
      let new_time = get_time () in
      Frame.master_of_seconds (new_time -. cur_time)
    end
  in
  let get_info () = Mad.get_output_format !dec in
  (get_info, get_data, seek)

module Make (Generator : Generator.S_Asio) = struct
  let create_decoder input =
    let resampler = Rutils.create_audio () in
    let get_info, get_data, seek = init input in
    {
      Decoder.seek;
      decode =
        (fun gen ->
          let data = get_data () in
          let sample_freq, _, _ = get_info () in
          let content = resampler ~audio_src_rate:(float sample_freq) data in
          Generator.set_mode gen `Audio;
          Generator.put_audio gen content 0 (Audio.length content));
    }
end

module G = Generator.From_audio_video
module Buffered = Decoder.Buffered (G)
module D = Make (G)

(** Configuration keys for mad. *)
let mime_types =
  Dtools.Conf.list
    ~p:(Decoder.conf_mime_types#plug "mad")
    "Mime-types used for guessing mpeg audio format"
    ~d:["audio/mpeg"; "audio/MPA"]

let file_extensions =
  Dtools.Conf.list
    ~p:(Decoder.conf_file_extensions#plug "mad")
    "File extensions used for guessing mpeg audio format"
    ~d:["mp3"; "mp2"; "mp1"]

(* Backward-compatibility keys.. *)
let () =
  ignore
    (mime_types#alias
       ~descr:
         "Mime-types used for guessing MP3 format (DEPRECATED, use *.mad \
          configuration keys!)"
       (Decoder.conf_mime_types#plug "mp3"));
  ignore
    (file_extensions#alias
       ~descr:
         "File extensions used for guessing MP3 format (DEPRECATED, use *.mad \
          configuration keys!)"
       (Decoder.conf_file_extensions#plug "mp3"))

let create_file_decoder filename kind =
  let generator = G.create `Audio in
  Buffered.file_decoder filename kind D.create_decoder generator

(* Get the number of channels of audio in a mpeg audio file.
 * This is done by decoding a first chunk of data, thus checking
 * that libmad can actually open the file -- which doesn't mean much. *)
let get_type filename =
  let fd = Mad.openfile filename in
  Tutils.finalize
    ~k:(fun () -> Mad.close fd)
    (fun () ->
      ignore (Mad.decode_frame_float fd);
      let f = Mad.get_frame_format fd in
      let layer =
        match f.Mad.layer with
          | Mad.Layer_I -> "I"
          | Mad.Layer_II -> "II"
          | Mad.Layer_III -> "III"
      in
      log#info
        "Libmad recognizes %S as mpeg audio (layer %s, %ikbps, %dHz, %d \
         channels)."
        filename layer (f.Mad.bitrate / 1000) f.Mad.samplerate f.Mad.channels;
      { Frame.audio = f.Mad.channels; video = 0; midi = 0 })

let () =
  Decoder.file_decoders#register "MAD" ~plugin_aliases:["MP3"; "MP2"; "MP1"]
    ~sdoc:
      "Use libmad to decode any file if its MIME type or file extension is \
       appropriate." (fun ~metadata:_ filename kind ->
      (* Before doing anything, check that we are allowed to produce
       * audio, and don't have to produce midi or video. Only then
       * check that the file seems relevant for decoding. *)
      if
        kind.Frame.audio = Frame.Zero
        || (not
              ( Frame.mul_sub_mul Frame.Zero kind.Frame.video
              && Frame.mul_sub_mul Frame.Zero kind.Frame.midi ))
        || not
             (Decoder.test_file ~mimes:mime_types#get
                ~extensions:file_extensions#get ~log filename)
      then None
      else if
        kind.Frame.audio = Frame.Variable
        || kind.Frame.audio = Frame.Succ Frame.Variable
        ||
        (* libmad always respects the first two kinds *)
        if Frame.type_has_kind (get_type filename) kind then true
        else begin
          log#important "File %S has an incompatible number of channels."
            filename;
          false
        end
      then Some (fun () -> create_file_decoder filename kind)
      else None)

module D_stream = Make (Generator.From_audio_video_plus)

let () =
  Decoder.stream_decoders#register "MAD" ~plugin_aliases:["MP3"; "MP2"; "MP1"]
    ~sdoc:"Use libmad to decode any stream with an appropriate MIME type."
    (fun mime kind ->
      let ( <: ) a b = Frame.mul_sub_mul a b in
      if
        List.mem mime mime_types#get
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
        Some D_stream.create_decoder
      else None)

let check filename =
  match Configure.file_mime with
    | Some f -> List.mem (f filename) mime_types#get
    | None -> (
        try
          ignore (get_type filename);
          true
        with _ -> false )

let duration file =
  if not (check file) then raise Not_found;
  let ans = Mad.duration file in
  match ans with 0. -> raise Not_found | _ -> ans

let () = Request.dresolvers#register "MAD" duration
