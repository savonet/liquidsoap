(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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

(** Plug for file decoding, in which [src/formats] plugins come. *)

open Dtools

let log = Log.make ["decoder"]

(** A decoder is a filling function and a closing function,
  * called as soon as a filling fails, i.e. is partial. *)
type decoder = {
  fill : Frame.t -> int ; (* Return remaining ticks. *)
  close : unit -> unit ;
}

(** Plugins are given filenames and return a decoder, if possible. *)
let formats : (string -> Frame.content_kind -> decoder option) Plug.plug =
  Plug.create
    ~doc:"Method to read audio files." ~insensitive:true "audio file formats"

let dummy =
  { fill = (fun b ->
      Frame.add_break b (Frame.position b) ;
      0) ;
    close = (fun _ -> ()) }

exception Exit of (string * (string -> Frame.content_kind -> decoder option))
(** Get a valid decoder creator for [filename].
  * The validity is not based on file extension but only on success of the
  * decoder instantiation.
  * Being based on file extension is weak, and troublesome when accessing a
  * remote file -- that would force us to create a local temporary file with the
  * same extension. *)
let search_valid filename kind : (string*(unit -> decoder)) option =
  try
    formats#iter ~rev:true 
      (fun format decoder ->
         log#f 4 "Trying %s decoder for %S..." format filename ;
         match decoder filename kind with
           | Some d ->
               d.close () ;
               log#f 3 "Decoder %s chosen for %S." format filename ;
               raise (Exit (format, decoder))
           | None  -> ()) ;
    log#f 3 "Unable to decode %S!" filename ;
    None
  with
    | Exit (format,d) ->
        Some (format,fun () ->
                match d filename kind with
                  | None ->
                      log#f 2 "Decoder %s betrayed us on %S!" format filename ;
                      dummy
                  | Some d -> d)
