(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

(** Read MIDI files.
  * The metadata support is TODO. *)

exception Invalid_header
exception Invalid_data

let log = Log.make ["decoder"; "midi"]

let decoder file =
  log#info "Decoding %s..." (Lang_string.quote_string file);
  let fd = new MIDI.IO.Reader.of_file file in
  let closed = ref false in
  let close () =
    assert (not !closed);
    closed := true;
    fd#close
  in
  let close_on_err f x =
    try f x
    with e ->
      log#info "Closing on error: %s." (Printexc.to_string e);
      close ();
      raise e
  in
  let fill buf =
    let m = MFrame.midi buf in
    let buflen = MFrame.size () in
    let r =
      close_on_err
        (fun () -> fd#read (Lazy.force Frame.midi_rate) m 0 buflen)
        ()
    in
    MFrame.add_break buf r;
    0
  in
  { Decoder.fill; fseek = (fun _ -> 0); close }

let () =
  Plug.register Decoder.decoders "midi" ~doc:"Decode midi files."
    {
      Decoder.media_type = `Midi;
      priority = (fun () -> 1);
      file_extensions = (fun () -> Some ["mid"]);
      mime_types = (fun () -> Some ["audio/midi"]);
      file_type =
        (fun ~metadata:_ ~ctype:_ _ ->
          Some
            (Frame.Fields.make
               ~midi:Content.(Midi.lift_params { Content.channels = 16 })
               ()));
      file_decoder =
        Some (fun ~metadata:_ ~ctype:_ filename -> decoder filename);
      stream_decoder = None;
    }
