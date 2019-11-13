(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

(** Read MIDI files.
  * The metadata support is TODO. *)

exception Invalid_header

exception Invalid_data

let log = Log.make ["decoder"; "midi"]

let decoder ~channels file =
  log#info "Decoding %S..." file ;
  let fd = new MIDI.IO.Reader.of_file file in
  let closed = ref false in
  let close () =
    assert (not !closed) ;
    closed := true ;
    fd#close
  in
  let close_on_err f x =
    try f x
    with e ->
      log#info "Closing on error: %s." (Printexc.to_string e) ;
      close () ;
      raise e
  in
  let fill buf =
    let m = MFrame.content_of_type ~channels buf 0 in
    let buflen = MFrame.size () in
    let r =
      close_on_err
        (fun () -> fd#read (Lazy.force Frame.midi_rate) m 0 buflen)
        ()
    in
    MFrame.add_break buf r ; 0
  in
  {Decoder.fill; fseek= (fun _ -> 0); close}

let () =
  Decoder.file_decoders#register "MIDI" (fun ~metadata:_ filename kind ->
      (* Any number of MIDI channel is acceptable as the decoder
       * silently drops events on higher channels if needed.
       * The number of MIDI channels is chosen at the beginning
       * independently of the actual file contents.
       * The kind should allow empty audio and video. *)
      let content =
        {(Frame.type_of_kind kind) with Frame.audio= 0; video= 0}
      in
      let channels = content.Frame.midi in
      if channels > 0 && Frame.type_has_kind content kind then
        Some (fun () -> decoder ~channels filename)
      else None)
