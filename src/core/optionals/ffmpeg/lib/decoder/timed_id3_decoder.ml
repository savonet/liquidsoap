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

let parse_id3 payload =
  match Metadata.Reader.with_string Metadata.ID3v2.parse payload with
    | tags -> Some tags
    | exception _ -> None

(* Some demuxers strip the ID3 header of timed ID3 payloads (FFmpeg ticket
   9540): it is rebuilt, version 3 first. *)
let parse payload =
  if String.length payload < 3 then Some []
  else if String.sub payload 0 3 = "ID3" then parse_id3 payload
  else (
    match parse_id3 ("ID3\x03\x00" ^ payload) with
      | Some tags -> Some tags
      | None -> parse_id3 ("ID3\x04\x00" ^ payload))

let create ~output ~faults =
  let push ~position:_ = function
    | `Data_packet packet -> (
        match parse (Avcodec.Packet.content packet) with
          | Some [] -> `Ok
          | Some tags ->
              output.Per_stream.add_metadata (Frame.Metadata.from_list tags);
              `Ok
          | None ->
              Faults.uncounted faults ~kind:"timed_id3"
                "Timed ID3 payload does not parse";
              `Ok)
    | _ -> `Ok
  in
  {
    Per_stream.sparse = true;
    push;
    switch = (fun ~source_id:_ ~continuing:_ -> ());
    advance = (fun _ -> ());
    flush = (fun () -> ());
  }
