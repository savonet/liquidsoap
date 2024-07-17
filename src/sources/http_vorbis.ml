(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

let vorbis_stream_log = Dtools.Log.make ["stream";"vorbis"]

let mime_types =
  Dtools.Conf.list ~p:(Http_source.conf_mime_types#plug "vorbis") 
    ~d:["application/ogg";"application/x-ogg";"audio/x-ogg"]
    "Mime types associated to Ogg/Vorbis format"

let decode_vorbis sink =
  let dec =
    Vorbis.Decoder.create
      (fun len ->
         try
           let data = sink.Http_source.read len in
             data, String.length data
         with
           | e ->
               vorbis_stream_log#f 2 "Read error %s" (Printexc.to_string e);
               "", 0
      )
      (fun _ _ -> -1)
      sink.Http_source.close
      (fun () -> -1)
  in
  let samplerate, channels =
    let info = Vorbis.Decoder.info dec (-1) in
      info.Vorbis.audio_samplerate,
      info.Vorbis.audio_channels
  in
  let metadata_of a =
    let h = Hashtbl.create (List.length a) in
      List.iter (fun (k,v) -> Hashtbl.add h (String.lowercase k) v) a ;
      h
  in
  let samples_per_frame = Fmt.samples_per_frame () in
  let bitstream = ref (-1) in

    while true do
      try
        if !bitstream <> Vorbis.Decoder.bitstream dec then
          (
            bitstream := Vorbis.Decoder.bitstream dec ;
            sink.Http_source.insert_metadata
              (metadata_of
                 (snd (Vorbis.Decoder.comments dec !bitstream)))
          );
        let buf = Vorbis.Decoder.decode_float_alloc dec samples_per_frame in
          sink.Http_source.put samplerate buf
      with
        | End_of_file as e -> Vorbis.Decoder.close dec ; raise e
        | Vorbis.Hole_in_data -> ()
    done

let () =
  ignore
    (Dtools.Init.at_start
       (fun () ->
          List.iter
            (fun s -> Http_source.stream_decoders#register s decode_vorbis)
            mime_types#get))
