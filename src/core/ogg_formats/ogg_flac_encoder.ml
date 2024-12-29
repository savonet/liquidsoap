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

external set_stream_eos : Ogg.Stream.stream -> unit
  = "liq_ocaml_ogg_stream_set_eos"

let create_encoder ~flac ~comments () =
  let samplerate = Lazy.force flac.Flac_format.samplerate in
  let p =
    {
      Flac.Encoder.channels = flac.Flac_format.channels;
      bits_per_sample = flac.Flac_format.bits_per_sample;
      sample_rate = samplerate;
      compression_level = Some flac.Flac_format.compression;
      total_samples = None;
    }
  in
  let enc = ref None in
  let started = ref false in
  let m = Mutex.create () in
  let pages = ref [] in
  let write_cb = Mutex_utils.mutexify m (fun p -> pages := p :: !pages) in
  let flush_pages =
    Mutex_utils.mutexify m (fun () ->
        let p = !pages in
        pages := [];
        List.rev p)
  in
  let get_enc os =
    match !enc with
      | Some x -> x
      | None ->
          let x =
            Flac_ogg.Encoder.create ~comments ~serialno:(Ogg.Stream.serialno os)
              ~write:write_cb p
          in
          enc := Some x;
          x
  in
  let empty_data () =
    Array.make (Lazy.force Frame.audio_channels) (Array.make 1 0.)
  in
  let header_encoder os =
    match get_enc os with
      | { Flac_ogg.Encoder.first_pages = p :: _ } -> p
      | _ -> raise Ogg_muxer.Invalid_data
  in
  let fisbone_packet os =
    Some
      (Flac_ogg.Skeleton.fisbone ~serialno:(Ogg.Stream.serialno os)
         ~samplerate:(Int64.of_int samplerate) ())
  in
  let stream_start os =
    match get_enc os with
      | { Flac_ogg.Encoder.first_pages = _ :: pages } -> pages
      | _ -> raise Ogg_muxer.Invalid_data
  in
  let data_encoder data os write_page =
    if not !started then started := true;
    let b, ofs, len =
      (data.Ogg_muxer.data, data.Ogg_muxer.offset, data.Ogg_muxer.length)
    in
    let b = Array.map (fun x -> Array.sub x ofs len) b in
    let { Flac_ogg.Encoder.encoder } = get_enc os in
    Flac.Encoder.process encoder b;
    List.iter write_page (flush_pages ())
  in
  let end_of_page p =
    let granulepos = Ogg.Page.granulepos p in
    if granulepos < Int64.zero then Ogg_muxer.Unknown
    else Ogg_muxer.Time (Int64.to_float granulepos /. float samplerate)
  in
  let end_of_stream os =
    let { Flac_ogg.Encoder.encoder } = get_enc os in
    (* Assert that at least some data was encoded.. *)
    if not !started then (
      let b = empty_data () in
      Flac.Encoder.process encoder b);
    Flac.Encoder.finish encoder;
    set_stream_eos os;
    flush_pages ()
  in
  {
    Ogg_muxer.header_encoder;
    fisbone_packet;
    stream_start;
    data_encoder = Ogg_muxer.Audio_encoder data_encoder;
    end_of_page;
    end_of_stream;
  }

let create_flac = function
  | Ogg_format.Flac flac ->
      let reset ogg_enc m =
        let comments =
          Frame.Metadata.to_list (Frame.Metadata.Export.to_metadata m)
        in
        let enc = create_encoder ~flac ~comments () in
        Ogg_muxer.register_track ?fill:flac.Flac_format.fill ogg_enc enc
      in
      let src_freq = float (Frame.audio_of_seconds 1.) in
      let dst_freq = float (Lazy.force flac.Flac_format.samplerate) in
      let channels = flac.Flac_format.channels in
      let encode = Ogg_encoder.encode_audio ~channels ~dst_freq ~src_freq () in
      { Ogg_encoder.encode; reset; id = None }
  | _ -> assert false

let () = Hashtbl.replace Ogg_encoder.audio_encoders "flac" create_flac
