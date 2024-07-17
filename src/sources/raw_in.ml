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

open Source
open Unix

class raw port =
object (self)
  inherit source

  method stype = Fallible

  val abg_max_len = 1000*Mixer.Buffer.size
  val lock = Mutex.create ()
  val abg = Mixer.Generator.create ()

  val polling = Mutex.create ()
  val mutable sleeping = true
  val mutable connected = false

  (* Feed the buffer generator *)
  val buf = String.create Mixer.Buffer.size
  method feeding ?(newstream=true) dec =
    connected <- true ;
    let again =
      Mutex.lock lock ;
      if Mixer.Generator.length abg >= abg_max_len then begin
        self#log 4 "Overflow: need to throw some data!" ;
        Mixer.Generator.remove abg Mixer.Buffer.size ;
        Mutex.unlock lock ;
        true
      end else
        let locked = ref true in
        try
          Mutex.unlock lock ;
          locked := false ;

          (* Write some WAV *)
          let r = Unix.read dec buf 0 Mixer.Buffer.size in
            (* Update buffer information *)
            Mutex.lock lock ;
            locked := true ;
            if r > 0 then
              Mixer.Generator.feed abg
                Mixer.Buffer.format
                (String.sub buf 0 r) ;
            Mutex.unlock lock ;
            locked := false ;
            true
        with
        | e ->
            if !locked then
              Mutex.unlock lock ;
            self#log 2
              (Dtools.Log.f "Decoding error: %s" (Printexc.to_string e)) ;
            (* TODO we loose some data at the end of a stream, near End_of_file
             * maybe there's a way to get it back, flush the decoder ? *)
            if e = End_of_file then false else true
    in
      if again && not sleeping then self#feeding ~newstream:false dec else begin
        self#log 3 "Feeding stopped. Closing decoder." ;
        Unix.close dec ;
        connected <- false
      end

  val mutable should_fail = false
  method abort_track = should_fail <- true

  method is_ready =
    connected ||
    (let r = Mutex.lock lock ; Mixer.Generator.length abg > 0 in
       Mutex.unlock lock ;
       r)

  method remaining =
    if should_fail then 0 else
      if connected then -1 else
        Mixer.Generator.length abg

  method get_frame ab =
    if should_fail then begin
      should_fail <- false ;
      Mixer.Buffer.add_break ab (Mixer.Buffer.position ab)
    end else
      let p = Mixer.Buffer.position ab in
        Mutex.lock lock ;
        if Mixer.Generator.length abg > 0 then
          Mixer.Buffer.fill ab abg
        else begin
          Mixer.Buffer.blankify ab p (Mixer.Buffer.size - p) ;
          Mixer.Buffer.add_break ab Mixer.Buffer.size
        end ;
        Mutex.unlock lock

  (* Called when there's no decoding process, in order to create one. *)
  method connect =
    self#log 3 "#connect" ;
    let max_conn = 1 in
    let bind_addr =
      ADDR_INET(inet_addr_of_string "127.0.0.1", port)
    in
    let sock = socket PF_INET SOCK_STREAM 0 in
      setsockopt sock SO_REUSEADDR true ;
      begin try bind sock bind_addr with
        | Unix.Unix_error(Unix.EADDRINUSE, "bind", "") ->
            failwith (Printf.sprintf "port %d already taken" port)
      end ;
      listen sock max_conn ;
      self#log 3 "Listening..." ;
      let (socket,_) = accept sock in
        self#log 3 "Accepted." ;
        self#feeding socket

  (* Take care of (re)starting the decoding *)
  method poll =
    (* Try to read the stream *)
    self#connect ;
    if sleeping then
      Mutex.unlock polling
    else
      ( Thread.delay 1. ; self#poll )

  method wake_up _ =
    (* Wait for the polling thread to return *)
    Mutex.lock polling ;
    sleeping <- false ;
    ignore (Tutils.create (fun () -> self#poll) () "http polling")

  method sleep =
    sleeping <- true

end

let _ =
    Lang.add_operator "input.raw"
      ~descr:"Receives raw audio over TCP."
      [ "", Lang.int_t, None, Some "Port" ]
      (fun p ->
         let port = Lang.to_int (List.assoc "" p) in
           ((new raw port):>source))
