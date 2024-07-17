(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

open Unix

let split_lines buf =
  Pcre.split ~pat:"\r\n" buf

let buflen = 1024

let read_file sink =
  let buf =
    let buf = ref "" in
    let n = ref 1 in
      try
        while !n > 0 do
          let s = sink.Http.read buflen in
            n := String.length s;
            buf := !buf ^ s
        done;
        !buf
      with
        | End_of_file -> !buf
        | e -> Dtools.Log.log ~label:"http_playlist" 2
                 (Dtools.Log.f "Read error %s"
                    (Printexc.to_string e));
               !buf
  in
    split_lines buf

let play sink urls =
  raise (Http.Playlist urls)

let decode_mpegurl sink =
  let lines = read_file sink in
  let urls =
    List.filter
      (fun s -> String.length s > 0 && s.[0] <> '#')
      lines
  in
    sink.Http.close ();
    play sink urls

let decode_scpls sink =
  let lines = read_file sink in
  let urls =
    List.map
      (fun s ->
         try
           let sub = Pcre.exec ~pat:"[Ff]ile\\d*[ ]*=[ ]*(.*)" s in
             Pcre.get_substring sub 1
         with
           | Not_found -> ""
      )
      lines
  in
  let urls = List.filter (fun s -> s <> "") urls in
    sink.Http.close ();
    play sink urls

let decode_asx sink =
  let lines = read_file sink in
  let urls =
    List.map
      (fun s ->
         try
           (* Ugly hack but I'm too lazy for a proper XML parser. *)
           let rex = Pcre.regexp ~flags:[`CASELESS] "[ ]*<[ ]*ref[ ]*href[ ]*=[ ]*\"(.*)\"[ ]*/>[ ]*" in
           let sub = Pcre.exec ~rex s in
             Pcre.get_substring sub 1
         with
           | Not_found -> ""
      )
      lines
  in
  let urls = List.filter (fun s -> s <> "") urls in
    sink.Http.close ();
    play sink urls

let decode_smil sink =
  let lines = read_file sink in
  let urls =
    List.map
      (fun s ->
         try
           (* Ugly hack but I'm too lazy for a proper XML parser. *)
           let rex = Pcre.regexp ~flags:[`CASELESS] "[ ]*<[ ]*audio[ ]*src[ ]*=[ ]*\"(.*)\"[ ]*/>[ ]*" in
           let sub = Pcre.exec ~rex s in
             Pcre.get_substring sub 1
         with
           | Not_found -> ""
      )
      lines
  in
  let urls = List.filter (fun s -> s <> "") urls in
    sink.Http.close ();
    play sink urls

let decode_xspf sink =
  let lines = read_file sink in
  let urls =
    List.map
      (fun s ->
         try
           (* Ugly hack but I'm too lazy for a proper XML parser. *)
           let rex = Pcre.regexp ~flags:[`CASELESS] ".*<location>(.*)</location>.*" in
           let sub = Pcre.exec ~rex s in
             Pcre.get_substring sub 1
         with
           | Not_found -> ""
      )
      lines
  in
  let urls = List.filter (fun s -> s <> "") urls in
    sink.Http.close ();
    play sink urls

let () =
  Http.stream_decoders#register "audio/x-mpegurl" decode_mpegurl;
  Http.stream_decoders#register "audio/x-scpls" decode_scpls;
  Http.stream_decoders#register "video/x-ms-asf" decode_asx;
  Http.stream_decoders#register "application/smil" decode_smil;
  Http.stream_decoders#register "application/xspf+xml" decode_xspf
