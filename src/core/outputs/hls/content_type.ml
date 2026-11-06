let playlist = "application/vnd.apple.mpegurl"

let ffmpeg_muxer = function
  | Encoder.Ffmpeg f -> f.Liquidsoap_ffmpeg_formats.Encoder_format.format
  | _ -> None

(* spec/serving.md §4.1: by the kind of file and its variant. *)
let segment v =
  match (v.Variant.format, ffmpeg_muxer v.format) with
    | format, _ when Variant.iso_muxer format -> "video/iso.segment"
    | (Encoder.MP3 _ | Encoder.Shine _), _ | _, Some "mp3" -> "audio/mpeg"
    | Encoder.FdkAacEnc _, _ | _, Some "adts" -> "audio/aac"
    | _, Some "mpegts" -> "video/mp2t"
    | _, Some "ac3" -> "audio/ac3"
    | _, Some "eac3" -> "audio/eac3"
    | _, Some "webvtt" -> "text/vtt"
    | format, _ -> Encoder.mime format

let has_video v =
  match v.Variant.format with
    | Encoder.Ffmpeg f ->
        List.exists
          (fun (field, stream) ->
            match stream with
              | `Encode
                  {
                    Liquidsoap_ffmpeg_formats.Encoder_format.options = `Video _;
                    _;
                  } ->
                  true
              | `Copy _ -> String.starts_with ~prefix:"video" field
              | _ -> false)
          f.streams
    | _ -> false

let init v = if has_video v then "video/mp4" else "audio/mp4"
