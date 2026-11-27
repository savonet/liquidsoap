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

(** FLAC encoder *)

let log = Log.make ["encoder"; "flac"]

(* The comments: the encoder identification on every sink, the tags on a file
   only. *)
let comments ~tags metadata =
  let legal (key, value) =
    let legal =
      Flac.Encoder.vorbiscomment_entry_name_is_legal key
      && Flac.Encoder.vorbiscomment_entry_value_is_legal value
    in
    if not legal then
      log#important "Metadata %S cannot be written as a FLAC comment: dropped"
        key;
    legal
  in
  let cover_keys = Metadata_settings.conf_meta_cover#get in
  let tags =
    if tags then
      List.filter
        (fun (key, _) ->
          let key = String.lowercase_ascii key in
          key <> "encoder" && not (List.mem key cover_keys))
        metadata
    else []
  in
  ("ENCODER", Configure.vendor) :: List.filter legal tags

let writer flac { Encoder.capabilities; sink; initial_metadata; _ } =
  let metadata =
    Frame.Metadata.to_list (Frame.Metadata.Export.to_metadata initial_metadata)
  in
  (* The file form: tags, a cover, and a STREAMINFO block completed at the
     end. *)
  let file_form = Encoder.file_form capabilities in
  let picture =
    if file_form then
      Option.map
        (fun { Utils.mime_type; picture } ->
          { Flac.Encoder.mime_type; picture })
        (Utils.cover_of_metadata metadata)
    else None
  in
  let channels = flac.Flac_format.channels in
  let samplerate_converter = Audio_converter.Samplerate.create channels in
  let samplerate = Deferred.force flac.Flac_format.samplerate in
  let src_freq = float (Frame.audio_of_seconds 1.) in
  let dst_freq = float samplerate in
  let p =
    {
      Flac.Encoder.channels;
      bits_per_sample = flac.Flac_format.bits_per_sample;
      sample_rate = samplerate;
      compression_level = Some flac.Flac_format.compression;
      total_samples = None;
    }
  in
  let buf = Strings.Mutable.empty () in
  (* Where the library is in the stream: it seeks back to complete its
     STREAMINFO block once it has written everything. *)
  let position = ref 0 in
  let write chunk =
    position := !position + Bytes.length chunk;
    Strings.Mutable.add_bytes buf chunk
  in
  let seek, tell =
    match sink.Encoder.seek with
      | Some seek when file_form ->
          ( Some
              (fun offset ->
                sink.Encoder.write (Strings.Mutable.flush buf);
                position := Int64.to_int offset;
                seek !position),
            Some (fun () -> Int64.of_int !position) )
      | _ -> (None, None)
  in
  let enc =
    Flac.Encoder.create
      ~comments:(comments ~tags:file_form metadata)
      ?picture ?seek ?tell ~write p
  in
  (* What the library writes when it is created: fLaC and its metadata
     blocks. *)
  let header = Strings.Mutable.flush buf in
  let pending_header = ref (Some header) in
  let take () =
    let bytes = Strings.Mutable.flush buf in
    match !pending_header with
      | Some header ->
          pending_header := None;
          Strings.append header bytes
      | None -> bytes
  in
  let encode frame =
    let b = AFrame.pcm frame in
    let len = AFrame.position frame in
    let b, start, len =
      Audio_converter.Samplerate.resample samplerate_converter
        (dst_freq /. src_freq) b 0 len
    in
    let b = Audio.sub b start len in
    Flac.Encoder.process enc b;
    take ()
  in
  let stop () =
    Flac.Encoder.finish enc;
    take ()
  in
  (* The library hands over whole frames: a call's output starts on one. *)
  Encoder.writer_of_encoder
    {
      Encoder.encode_metadata = (fun _ -> ());
      header = (fun () -> header);
      encode;
      stop;
      bitrate = (fun () -> None);
      access_points = None;
      restart_offset = (fun () -> None);
    }
    sink

let () =
  Plug.register Encoder.plug "flac" ~doc:"Flac encoder." (function
    | Encoder.Flac m -> Some (writer m)
    | _ -> None)
