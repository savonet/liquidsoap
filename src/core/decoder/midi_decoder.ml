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

open Mm

(** Read MIDI files. The metadata support is TODO. *)

exception Invalid_header
exception Invalid_data

let log = Log.make ["decoder"; "midi"]

let decoder ~ctype file =
  log#info "Decoding %s..." (Lang_string.quote_string file);
  let fd = new MIDI.IO.Reader.of_file file in
  let closed = ref false in
  let fclose () =
    assert (not !closed);
    closed := true;
    fd#close
  in
  let close_on_err f x =
    try f x
    with e ->
      log#info "Closing on error: %s." (Printexc.to_string e);
      fclose ();
      raise e
  in
  let fread length =
    let frame = Frame.create ~length ctype in
    let m = Content.Midi.get_data (Frame.get frame Frame.Fields.midi) in
    let r =
      close_on_err
        (fun () -> fd#read (Lazy.force Frame.midi_rate) m 0 length)
        ()
    in
    Frame.set_data (Frame.slice frame r) Frame.Fields.midi
      Content.Midi.lift_data m
  in
  { Decoder.fread; remaining = (fun _ -> 0); fseek = (fun _ -> 0); fclose }

let () =
  Plug.register Decoder.decoders "midi" ~doc:"Decode midi files."
    {
      Decoder.priority = (fun () -> 1);
      file_extensions = (fun () -> Some ["mid"]);
      mime_types = (fun () -> Some ["audio/midi"]);
      file_type =
        (fun ~metadata:_ ~ctype:_ _ ->
          Some
            (Frame.Fields.make
               ~midi:Content.(Midi.lift_params { Content.Midi.channels = 16 })
               ()));
      file_decoder =
        Some (fun ~metadata:_ ~ctype filename -> decoder ~ctype filename);
      stream_decoder = None;
    }
