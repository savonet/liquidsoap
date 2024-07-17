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

let log = Dtools.Log.make ["rtp"]

type session
type mode = Recv | Send

type t = {
  session : session ;
  mutable t0 : float ;
  mutable t : int64 ;
}

external _new_session : mode -> string -> int -> int -> session =
  "liquidsoap_new_session"

let new_session ?(ttl=0) mode ip port = {
  session = _new_session mode ip port ttl ;
  t0 = 0. ;
  t = 0L
}

external _send     : session -> string -> int = "liquidsoap_send_buffer"
external _recv     : session -> string -> int = "liquidsoap_recv_buffer"
external meta_recv : session -> string        = "liquidsoap_recv_metadata"
external meta_send : session -> string -> int = "liquidsoap_send_metadata"

let encode h =
  let encode s ss =
    let l  = String.length s in
    let ll = String.length ss in
      if l>255 || ll>255 then "" else
        ((String.make 1 (char_of_int l))^s^
         (String.make 1 (char_of_int ll))^ss)
  in
    Hashtbl.fold (fun k v s -> s^(encode k v)) h ""

let decode s =
  let n = String.length s in
  let rec to_list offset acc =
    if offset = n then acc else
      let size = int_of_char s.[offset] in
      let w = String.sub s (offset+1) size in
        to_list (offset+1+size) (w::acc)
  in
  let to_list n l = try to_list n l with _ -> [] in
  let t = Array.of_list (to_list 0 []) in
  let l = (Array.length t)/2 in
  let h = Hashtbl.create l in
    for i = 0 to l-1 do
      Hashtbl.add h t.(2*i+1) t.(2*i)
    done ;
    h

let delay = Fmt.seconds_per_frame ()

let lastlog = ref (Unix.time ())
let sync t =
  if t.t = 0L then
    ( t.t <- 1L ;
      t.t0 <- Unix.gettimeofday () )
  else
    let d = (t.t0+.(Int64.to_float t.t)*.delay) -.
            (Unix.gettimeofday ()) in
      if d>0. then
        ignore (Unix.select [] [] [] d)
      else
        if Unix.time () -. !lastlog > 1. then
          ( lastlog := Unix.time () ;
            Printf.fprintf stderr "We must catchup %f seconds\n%!" (-.d) ) ;
      t.t <- Int64.add t.t 1L

open Dtools

exception Have_more
exception Error

let send ?(nosync=false) t b =
  assert (not (AFrame.is_partial b)) ;
  if not nosync then sync t ;
  List.iter
    (fun (s,m) ->
       log#f 4 "Sending metadata packet" ;
       Hashtbl.add m "timestamp" (string_of_int s) ;
       if meta_send t.session (encode m) <= 0 then raise Error)
    (AFrame.get_all_metadata b) ;
  if _send t.session (AFrame.to_s16le b) <= 0 then raise Error

let recv ?(nosync=false) t b =
  AFrame.free_metadata b ;
  if not nosync then sync t ;
  let s = meta_recv t.session in
    if s <> "" then begin
      log#f 4 "Received metadata packet %S" s ;
      let h = decode s in
        try
          AFrame.set_metadata b
            (int_of_string (Hashtbl.find h "timestamp")) h
        with Not_found -> assert false
    end ;
    let buf = AFrame.to_s16le b in (* TODO: allocate a fresh buffer of the right size *)
    let ret = _recv t.session buf in
      Float_pcm.from_s16le (AFrame.get_float_pcm b) 0 buf 0 (String.length buf / 4);
      (* 1 means have_more, -1 means that an error occured *)
      match ret with
        | 1  -> raise Have_more
        | -1 -> raise Error
        | n  -> assert (n=0)

(* TODO close sessions *)
