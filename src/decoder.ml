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

(** Plug for file decoding, in which [src/formats] plugins come. *)

open Dtools

(** A decoder is a filling function and a closing function,
  * called as soon as a filling fails -- is not complete. *)
type decoder = {
  fill : Mixer.Buffer.t -> int ;
  close : unit -> unit ;
}

(** Plugins are given filenames and return a decoder, if possible. *)
let formats : (string -> decoder option) Plug.plug =
  Plug.create ~doc:"Method to read audio files." ~insensitive:true "formats"

let pref = ref []
let _ =
  (* TODO ref := custom format testing order *)
  Dtools.Init.at_start (fun () -> pref := formats#keys)

exception Exit of (string -> decoder option)
(** Get a valid decoder creator for [filename].
  * Returns the name of the decoder kind (ogg,...), and the decoder itself.
  * The returned creator is guaranteed to create decoders
  * (1) quickly (within less than 0.5s),
  * (2) and which have some data to deliver.
  * This is used to guarantee that the files opened for decoding in the
  * main thread can yield a non-empty track, and won't block the main thread. *)
let search_valid : string -> (unit -> decoder) option =
  let tester = Mixer.Buffer.create () in
  let testing = Mutex.create () in
    fun filename ->
      let iter = if !pref = [] then formats#iter else
        fun f ->
          List.iter (fun fmt ->
                       match formats#get fmt with
                       | None -> () | Some d -> f fmt d) !pref
      in
        try
          iter
            (fun format decoder ->
               let limit = 0.5 +. Unix.gettimeofday () in
               Log.logl ~label:"decoder" 4
                 (lazy (Log.f "Trying %s decoder for %S" format filename)) ;
               match decoder filename with
               | Some d ->
                   if Unix.gettimeofday () > limit then d.close () else begin
                     Mutex.lock testing ;
                     Mixer.Buffer.free tester ;
                     ignore (d.fill tester) ;
                     d.close () ;
                     if Unix.gettimeofday () <= limit &&
                        Mixer.Buffer.position tester <> 0 then begin
                       Mutex.unlock testing ;
                       raise (Exit decoder)
                     end else
                       Mutex.unlock testing
                   end
               | None  -> ()) ;
          Log.log ~label:"decoder" 3
            (Log.f "Unable to decode %S" filename) ;
          None
        with
        | Exit d ->
            Some (fun () ->
                    match d filename with
                      | None -> assert false
                      | Some d -> d)

(* This is overkill, we actually don't care about the extra checks here. *)
let get f =
  match search_valid f with
  | None -> None
  | Some d -> Some (d ())
