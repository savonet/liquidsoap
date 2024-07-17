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

open Types
open Unix

exception Internal

let url_expr = Str.regexp "^http://\\([^/]+\\)\\(/.+\\)$"
let host_expr = Str.regexp "^\\([^:]+\\):\\([0-9]+\\)$"
let convert_info i =
  { Mixer.Buffer.format with
      Mixer.channels = i.Vorbis.audio_channels ;
      Mixer.sample_freq = i.Vorbis.audio_sample_rate }

let metadata_of a =
  let h = Hashtbl.create (Array.length a) in
    Array.iter (fun (k,v) -> Hashtbl.add h (String.lowercase k) v) a ;
    h

class http url =
  (* Url format: <http://host/mount> or <http://host:port/mount> *)
  let host,port,mount =
    let host,mount =
      assert (Str.string_match url_expr url 0) ;
      (Str.matched_group 1 url),(Str.matched_group 2 url)
    in
      if Str.string_match host_expr host 0 then
        (Str.matched_group 1 host),
        (int_of_string (Str.matched_group 2 host)),
        mount
      else
        host,8000,mount
  in
  let get = Printf.sprintf "GET %s HTTP/1.0\r\n\r\n" mount in
  let get_length = String.length get in
object (self)
  inherit source

  method stype = Fallible

  val abg_max_len = 1000*Mixer.Buffer.size
  val lock = Mutex.create ()
  val abg = Mixer.Generator.create ()
  val mutable metadata = Hashtbl.create 0
  val mutable bitstream = -1
  val mutable metadata_pos = -1

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
          (* Get the metadata *)
          if newstream || bitstream <> Vorbis.get_dec_file_bitstream dec then
            begin
              bitstream <- Vorbis.get_dec_file_bitstream dec ;
              metadata <- metadata_of
                            (snd (Vorbis.get_dec_file_comments dec None)) ;
              metadata_pos <- Mixer.Generator.length abg ;
              self#log 3 (Dtools.Log.f "New metadata chunk %S"
                            (try Hashtbl.find metadata "title" with _ -> "?"))
            end ;
          Mutex.unlock lock ;
          locked := false ;

          (* Write some WAV *)
          let r = Vorbis.decode dec buf 0 Mixer.Buffer.size in
            (* Update buffer information *)
            Mutex.lock lock ;
            locked := true ;
            if r > 0 then
              Mixer.Generator.feed abg
                (convert_info (Vorbis.get_dec_file_info dec))
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
        Vorbis.close_dec_file dec ;
        connected <- false
      end

  val mutable should_fail = false
  method abort_track = should_fail <- true

  method is_ready =
    connected ||
    (let r = Mutex.lock lock ; Mixer.Generator.length abg > 0 in
       Mutex.unlock lock ;
       r)

  method get_frame ab =
    if should_fail then begin
      should_fail <- false ;
      Mixer.Buffer.add_break ab (Mixer.Buffer.position ab)
    end else
      let p = Mixer.Buffer.position ab in
        Mutex.lock lock ;
        if Mixer.Generator.length abg > 0 then begin
          if 0 <= metadata_pos && metadata_pos < Mixer.Buffer.size - p then
            begin
              Mixer.Buffer.set_metadata ab metadata_pos metadata ;
              self#log 3 (Dtools.Log.f "Metadata %S put to buffer at %d"
                            (try Hashtbl.find metadata "title" with _ -> "?")
                            metadata_pos)
            end ;
          Mixer.Buffer.fill ab abg ;
          metadata_pos <- metadata_pos - ((Mixer.Buffer.position ab)-p)
        end else begin
          Mixer.Buffer.blankify ab p (Mixer.Buffer.size - p) ;
          Mixer.Buffer.add_break ab Mixer.Buffer.size
        end ;
        Mutex.unlock lock

  (* Called when there's no decoding process, in order to create one. *)
  method connect =
    self#log 4
      (Dtools.Log.f "Trying to connect to http://%s:%d%s" host port mount) ;
    let socket = socket PF_INET SOCK_STREAM 0 in try
      connect socket (ADDR_INET((gethostbyname host).h_addr_list.(0), port)) ;
      try
        if Unix.write socket get 0 get_length < get_length then raise Internal ;
        let header = String.create 4096 in
        let f = Mixer.Buffer.format in
        let h = Unix.read socket header 0 4096 in
          if h < 12 then failwith "No header to read" else
            ( self#log 4 (String.sub header 0 (min h 12)) ;
              if String.sub header 9 3 <> "200" then raise Internal ) ;
          self#log 4
            (Dtools.Log.f "Got header from http://%s:%d%s" host port mount) ;
          let dec =
            Vorbis.open_dec_stream
              (fun len ->
                 let b = String.create len in
                   try
                     let r = Unix.read socket b 0 len in
                       if r < 0 then "" else String.sub b 0 r
                   with
                     | e -> self#log 2 (Dtools.Log.f "Read error %S"
                                          (Printexc.to_string e)) ;
                            "")
              (fun () -> -1)
              (fun () ->
                 shutdown socket SHUTDOWN_ALL ;
                 close socket )
              (fun () -> -1)
              { Vorbis.sample_size = f.Mixer.sample_size ;
                Vorbis.big_endian = f.Mixer.big_endian ;
                Vorbis.signed = f.Mixer.signed }
          in
            self#log 3 "Connected! Decoding..." ;
            self#feeding dec
      with
      | e ->
          self#log 4
            (Dtools.Log.f "Connection failed: %S" (Printexc.to_string e)) ;
          shutdown socket SHUTDOWN_ALL ; close socket
    with
    | e ->
        self#log 4
          (Dtools.Log.f "Connection failed: %S" (Printexc.to_string e)) ;
        close socket

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
    Lang.add_operator "input.http"
      ~descr:"Forwards the given ogg/vorbis http stream"
      [ "", Lang.string_t, None, Some "URL of an http ogg stream." ]
      (fun p ->
         let url = Lang.to_string (List.assoc "" p) in
           ((new http url):>source))
