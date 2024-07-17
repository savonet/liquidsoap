(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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

let decode_vorbis sink =
  let dec =
    Vorbis.open_dec_stream
      (fun len ->
         try
           sink.Http.read len
         with
           | e -> Dtools.Log.log ~label:"vorbis_stream" 2
                    (Dtools.Log.f "Read error %s"
                       (Printexc.to_string e)) ;
                  "")
      (fun () -> -1)
      sink.Http.close
      (fun () -> -1)
  in
  let samplerate =
    let info = Vorbis.get_dec_file_info dec in
      info.Vorbis.audio_sample_rate
  in
  let metadata_of a =
    let h = Hashtbl.create (Array.length a) in
      Array.iter (fun (k,v) -> Hashtbl.add h (String.lowercase k) v) a ;
      h
  in
  let samples_per_frame = Fmt.samples_per_frame () in
  let bitstream = ref (-1) in

    while true do
      try
        if !bitstream <> Vorbis.get_dec_file_bitstream dec then begin
          bitstream := Vorbis.get_dec_file_bitstream dec ;
          sink.Http.insert_metadata
            (metadata_of
               (snd (Vorbis.get_dec_file_comments dec None)))
        end ;
        sink.Http.put samplerate
          (Vorbis.decode_float dec samples_per_frame)
      with
        | End_of_file as e -> Vorbis.close_dec_file dec ; raise e
        | Vorbis.Hole_in_data -> ()
    done

let () =
  Http.stream_decoders#register "application/ogg" decode_vorbis
