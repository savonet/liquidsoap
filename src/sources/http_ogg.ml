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

let log = Dtools.Log.make ["stream";"ogg"]

exception Exit of (string -> Http_source.sink -> unit)

let mimes = ["application/ogg";"application/x-ogg";"audio/x-ogg"]
let mime_types =
  Dtools.Conf.list ~p:(Http_source.conf_mime_types#plug "ogg")
    ~d:mimes "Mime types associated to Ogg container"
let _ =
  Dtools.Conf.list ~p:(Http_source.conf_mime_types#plug "vorbis")
    ~d:mimes "Mime types associated to Ogg container. \n\
      This settings has been DEPRECATED."

let decode_ogg sink = 
  let feed len = 
    let data = sink.Http_source.read len in
    data, String.length data
  in
  let sync = Ogg.Sync.create feed in
    let decoder = Ogg_demuxer.init sync in
    try
      while true do
          let feed ((buf,samplerate),com) = 
            begin
              match com with
                | Some meta ->
                    sink.Http_source.insert_metadata meta
                | _ -> ()
            end;
            sink.Http_source.put samplerate buf
          in
          try
            Ogg_demuxer.decode_audio decoder feed;
            if !(decoder.Ogg_demuxer.eos) then
              Ogg_demuxer.reset decoder
          with
            | Ogg_demuxer.End_of_stream -> () (* We could handle breaks perhaps.. *)
      done
    with
      | e -> sink.Http_source.close (); raise e
    
let () =
  ignore
    (Dtools.Init.at_start
       (fun () ->
          List.iter
            (fun s -> Http_source.stream_decoders#register s decode_ogg)
            mime_types#get))


