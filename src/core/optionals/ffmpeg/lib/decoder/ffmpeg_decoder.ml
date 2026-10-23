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

let log = Log.make ["decoder"; "ffmpeg"]

let extension filename =
  try Some (String.lowercase_ascii (Utils.get_ext filename)) with _ -> None

let is_image_extension filename =
  match extension filename with
    | Some ext -> List.mem ext Decoder_settings.image_file_extensions#get
    | None -> false

let mime_base mime = List.hd (String.split_on_char ';' mime)

let is_image_mime mime =
  List.mem (mime_base mime) Decoder_settings.image_mime_types#get

let configure_stream (type media) ~find ~string_of_id ~get_params_id :
    media Avcodec.params -> media Av.stream_config =
 fun params ->
  let name = string_of_id (get_params_id params) in
  match Decoder_settings.preferred_decoder name with
    | None -> { Av.codec = None; opts = None }
    | Some decoder ->
        log#info "Trying preferred decoder %s for codec %s" decoder name;
        { Av.codec = Some (find decoder); opts = None }

type stream_configuration = {
  audio : Avutil.audio Avcodec.params -> Avutil.audio Av.stream_config;
  video : Avutil.video Avcodec.params -> Avutil.video Av.stream_config;
  subtitle : Avutil.subtitle Avcodec.params -> Avutil.subtitle Av.stream_config;
}

let stream_configuration () =
  {
    audio =
      configure_stream ~find:Avcodec.Audio.find_decoder_by_name
        ~string_of_id:Avcodec.Audio.string_of_id
        ~get_params_id:Avcodec.Audio.get_params_id;
    video =
      configure_stream ~find:Avcodec.Video.find_decoder_by_name
        ~string_of_id:Avcodec.Video.string_of_id
        ~get_params_id:Avcodec.Video.get_params_id;
    subtitle =
      configure_stream ~find:Avcodec.Subtitle.find_decoder_by_name
        ~string_of_id:Avcodec.Subtitle.string_of_id
        ~get_params_id:Avcodec.Subtitle.get_params_id;
  }

let open_file ?(configure = true) ~options filename =
  let { Input_options.format; opts } = options in
  let container =
    if configure then (
      let c = stream_configuration () in
      Av.open_input ?format ~opts ~configure_audio_stream:c.audio
        ~configure_video_stream:c.video ~configure_subtitle_stream:c.subtitle
        filename)
    else Av.open_input ?format ~opts filename
  in
  (match Input_options.check_unused options with
    | () -> ()
    | exception exn ->
        Av.close container;
        raise exn);
  container

let file_options ~metadata filename =
  let options = Input_options.of_metadata metadata in
  if is_image_extension filename then Input_options.add_image_options options;
  options

let describe ~options ~url container =
  let description =
    Stream_description.container ?format:options.Input_options.format ~url
      container
  in
  log#important "FFmpeg recognizes %S as %s" url
    (Stream_description.describe description);
  description

let first_video_field ctype =
  List.find_opt
    (fun (field, _) ->
      String.length (Frame.Fields.string_of_field field) >= 5
      && String.sub (Frame.Fields.string_of_field field) 0 5 = "video")
    (Frame.Fields.bindings ctype)

let file_type ~metadata ~ctype filename =
  match first_video_field ctype with
    | Some (_, format)
      when is_image_extension filename && Content.Video.is_format format ->
        Some Frame.Fields.empty
    | _ ->
        let options = file_options ~metadata filename in
        let container = open_file ~options filename in
        Fun.protect
          ~finally:(fun () -> Av.close container)
          (fun () ->
            let description = describe ~options ~url:filename container in
            let decoded = Stream_description.get_type ~ctype description in
            if not (Decode_loop.can_map ~ctype description) then
              failwith "Content type not decodable by this build";
            Some decoded)

let create_file_decoder ~metadata ~ctype filename =
  let options = file_options ~metadata filename in
  let container = open_file ~options filename in
  match
    let description =
      Stream_description.container ?format:options.Input_options.format
        ~url:filename container
    in
    ignore (Stream_description.get_type ~ctype description);
    Decode_loop.create ~request_metadata:metadata ~decode_first_metadata:false
      ~ctype ~description container
  with
    | loop ->
        Decoder.file_decoder ~filename
          ~remaining:(fun () -> Decode_loop.remaining loop)
          ~ctype (Decode_loop.decoder loop)
    | exception exn ->
        Av.close container;
        raise exn

(* The current byte position of a stream input, from the last seek plus every
   byte read since. *)
let stream_io (input : Decoder.input) =
  let position = ref 0 in
  let read buffer offset length =
    let n = input.read buffer offset length in
    position := !position + n;
    n
  in
  let seek =
    Option.map
      (fun lseek offset (whence : Unix.seek_command) ->
        let target =
          match whence with
            | Unix.SEEK_SET -> Some offset
            | Unix.SEEK_CUR -> Some (!position + offset)
            | Unix.SEEK_END -> (
                match input.length with
                  | Some length -> Some (length () + offset)
                  | None -> None)
        in
        match target with
          | None -> -1
          | Some target -> (
              match lseek target with
                | p ->
                    position := p;
                    p
                | exception _ -> -1))
      input.lseek
  in
  (read, seek)

let create_stream_decoder ~ctype mime input =
  let options = Input_options.of_mime mime in
  if is_image_mime mime then Input_options.add_image_options options;
  let read, seek = stream_io input in
  let { Input_options.format; opts } = options in
  let container = Av.open_input_stream ?format ~opts ?seek read in
  Input_options.check_unused options;
  let description = describe ~options ~url:mime container in
  ignore (Stream_description.get_type ~ctype description);
  Decode_loop.decoder
    (Decode_loop.create ~decode_first_metadata:true ~ctype ~description
       container)

let duration ~metadata filename =
  let options = Input_options.of_metadata metadata in
  let container = open_file ~configure:false ~options filename in
  Fun.protect
    ~finally:(fun () -> Av.close container)
    (fun () ->
      match Av.get_input_duration ~format:`Millisecond container with
        | Some d -> Int64.to_float d /. 1000.
        | None -> raise Not_found)

let hms_duration value =
  match String.split_on_char ':' value with
    | [h; m; s] -> (
        match
          (int_of_string_opt h, int_of_string_opt m, float_of_string_opt s)
        with
          | Some h, Some m, Some s ->
              Some
                (Printf.sprintf "%g"
                   ((float h *. 3600.) +. (float m *. 60.) +. s))
          | _ -> None)
    | _ -> None

let tags ~metadata filename =
  let options = Input_options.of_metadata metadata in
  let container = open_file ~configure:false ~options filename in
  Fun.protect
    ~finally:(fun () -> Av.close container)
    (fun () ->
      let stream_tags =
        match Av.find_best_audio_stream container with
          | _, stream, _ -> Av.get_metadata stream
          | exception _ -> []
      in
      List.filter_map
        (fun (key, value) ->
          let key = if key = "track" then "tracknumber" else key in
          if String.lowercase_ascii key = "duration" then (
            match (float_of_string_opt value, hms_duration value) with
              | Some _, _ -> Some (key, value)
              | None, Some seconds -> Some (key, seconds)
              | None, None ->
                  log#important "Ignoring invalid duration tag %S" value;
                  None)
          else Some (key, value))
        (stream_tags @ Av.get_input_metadata container))

let metadata_resolver ~metadata ~extension ~mime filename =
  let accepted =
    (match extension with
      | Some ext -> List.mem ext Decoder_settings.file_extensions#get
      | None -> false)
    || List.exists
         (fun m -> mime_base m = mime_base mime)
         Decoder_settings.mime_types#get
  in
  if not accepted then []
  else (
    try tags ~metadata filename
    with exn ->
      log#important "Cannot read the tags of %S: %s" filename
        (Printexc.to_string exn);
      [])

let () =
  Plug.register Decoder.decoders "ffmpeg"
    ~doc:
      "Use FFmpeg to decode any file or stream if its MIME type or file \
       extension is appropriate."
    {
      Decoder.priority = (fun () -> Decoder_settings.priority#get);
      file_extensions =
        (fun () ->
          Some
            (Decoder_settings.file_extensions#get
           @ Decoder_settings.image_file_extensions#get));
      mime_types =
        (fun () ->
          Some
            (Decoder_settings.mime_types#get
           @ Decoder_settings.image_mime_types#get));
      file_type;
      file_decoder = Some create_file_decoder;
      stream_decoder = Some create_stream_decoder;
    };
  Plug.register Request.dresolvers "ffmpeg" ~doc:""
    {
      Request.dpriority = (fun () -> Decoder_settings.priority#get);
      file_extensions = (fun () -> Decoder_settings.file_extensions#get);
      dresolver = duration;
    };
  Plug.register Request.mresolvers "ffmpeg" ~doc:""
    {
      Request.priority = (fun () -> Decoder_settings.metadata_priority#get);
      resolver = metadata_resolver;
    }
