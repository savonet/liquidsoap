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

(** Decode mpeg audio files using libmad. *)

let log = Log.make ["decoder"; "mad"]

let init input =
  let index = Hashtbl.create 10 in
  let time_offset = ref 0 in
  let dec = ref (Mad.openstream input.Decoder.read) in
  (* Skip id3 tags if possible. *)
  begin match (input.Decoder.lseek, input.Decoder.tell) with
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
          if not (Hashtbl.mem index time) then Hashtbl.replace index time (f ())
  in
  (* Add an initial index. *)
  update_index ();
  let get_data () =
    let data = Mad.decode_frame_float !dec in
    update_index ();
    data
  in
  let get_time () =
    float !time_offset
    +. (float (Mad.get_current_time !dec Mad.Centiseconds) /. 100.)
  in
  let seek ticks =
    if ticks < 0 && input.Decoder.lseek = None then 0
    else (
      let time = Frame.seconds_of_main ticks in
      let cur_time = get_time () in
      let seek_time = cur_time +. time in
      let seek_time = if seek_time < 0. then 0. else seek_time in
      if time < 0. then (
        try
          let seek_time = int_of_float (floor seek_time) in
          let seek_pos = if seek_time > 0 then get_index seek_time else 0 in
          ignore ((Option.get input.Decoder.lseek) seek_pos);
          dec := Mad.openstream input.Decoder.read;

          (* Decode one frame to set the decoder to a good reading position
           * on next read. *)
          ignore (Mad.decode_frame_float !dec);

          (* We have to assume here that new_pos = seek_pos.. *)
          time_offset := seek_time
        with _ -> ());
      let rec f pos =
        if pos < seek_time then
          if
            try
              Mad.skip_frame !dec;
              true
            with Mad.End_of_stream -> false
          then (
            update_index ();
            f (get_time ()))
      in
      f (get_time ());
      let new_time = get_time () in
      Frame.main_of_seconds (new_time -. cur_time))
  in
  let get_info () = Mad.get_frame_format !dec in
  (get_info, get_data, seek)

let create_decoder input =
  let get_info, get_data, seek = init input in
  {
    Decoder.seek;
    decode =
      (fun buffer ->
        let data = get_data () in
        let { Mad.samplerate } = get_info () in
        buffer.Decoder.put_pcm ~samplerate data);
    eof = (fun _ -> ());
    close = (fun _ -> ());
  }

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

let priority =
  Dtools.Conf.int
    ~p:(Decoder.conf_priorities#plug "mad")
    "Priority for the mpeg audio decoder" ~d:1

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

(* Get the number of channels of audio in a mpeg audio file.
   This is done by decoding a first chunk of data, thus checking
   that libmad can actually open the file -- which doesn't mean much. *)
let file_type filename =
  let fd = Mad.openfile filename in
  Fun.protect
    ~finally:(fun () -> Mad.close fd)
    (fun () ->
      ignore (Mad.decode_frame_float fd);
      let f = Mad.get_frame_format fd in
      let layer =
        match f.Mad.layer with
          | Mad.Layer_I -> "I"
          | Mad.Layer_II -> "II"
          | Mad.Layer_III -> "III"
      in
      log#important
        "Libmad recognizes %S as mpeg audio (layer %s, %ikbps, %dHz, %d \
         channels)."
        filename layer (f.Mad.bitrate / 1000) f.Mad.samplerate f.Mad.channels;
      Some
        (Frame.Fields.make
           ~audio:(Content.Audio.format_of_channels f.Mad.channels)
           ()))

let create_file_decoder ~metadata:_ ~ctype filename =
  Decoder.opaque_file_decoder ~filename ~ctype create_decoder

let () =
  Plug.register Decoder.decoders "mad"
    ~doc:
      "Use libmad to decode any file if its MIME type or file extension is \
       appropriate."
    {
      Decoder.priority = (fun () -> priority#get);
      file_extensions = (fun () -> Some file_extensions#get);
      mime_types = (fun () -> Some mime_types#get);
      file_type = (fun ~metadata:_ ~ctype:_ f -> file_type f);
      file_decoder = Some create_file_decoder;
      stream_decoder = Some (fun ~ctype:_ _ -> create_decoder);
    }

let check filename =
  List.mem (Magic_mime.lookup filename) mime_types#get
  ||
    try
      ignore (file_type filename);
      true
    with _ -> false

let dresolver ~metadata:_ file =
  if not (check file) then raise Not_found;
  let ans = Mad.duration file in
  match ans with 0. -> raise Not_found | _ -> ans

let () =
  Plug.register Request.dresolvers "mad"
    ~doc:"Compute duration of mp3 files using MAD library."
    {
      dpriority = (fun () -> priority#get);
      file_extensions = (fun () -> file_extensions#get);
      dresolver;
    }
