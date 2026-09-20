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
open Content_base

(* The settings are core's, and this is where the two meet. *)
let () =
  Midi_format.channels :=
    fun () -> Lazy.Mutexed.force Frame_settings.midi_channels

module Data = struct
  open Frame_settings

  type params = Midi_format.Specs.params
  type data = MIDI.Multitrack.t

  let blit src src_pos dst dst_pos len =
    let ( ! ) = midi_of_main in
    Array.iter2 (fun m m' -> MIDI.blit m !src_pos m' !dst_pos !len) src dst

  let copy m = Array.map MIDI.copy m
  let params m = { Midi_format.Specs.channels = MIDI.Multitrack.channels m }

  let make ?(length = 0) { Midi_format.Specs.channels } =
    MIDI.Multitrack.create channels (midi_of_main length)

  let length d = main_of_midi (MIDI.Multitrack.duration d)

  let checksum d =
    (* Hash MIDI data: number of channels and events per channel *)
    let channels = MIDI.Multitrack.channels d in
    let duration = MIDI.Multitrack.duration d in
    let channel_info =
      Array.to_list
        (Array.mapi
           (fun i track ->
             let events = MIDI.data track in
             let event_hash =
               List.fold_left
                 (fun acc (time, evt) ->
                   Printf.sprintf "%s;%d:%d" acc time (Hashtbl.hash evt))
                 "" events
             in
             Printf.sprintf "%d:%s" i event_hash)
           d)
    in
    Digest.string
      (Printf.sprintf "%d:%d:%s" channels duration
         (String.concat "|" channel_info))
    |> Digest.to_hex
end

include MkDataBase (Midi_format.Format) (Data)
include Midi_format
