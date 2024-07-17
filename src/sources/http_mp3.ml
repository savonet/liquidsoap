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

exception Internal

let url_expr = Str.regexp "^http://\\([^/]+\\)\\(/.+\\)$"
let host_expr = Str.regexp "^\\([^:]+\\):\\([0-9]+\\)$"

let metaint_of_header h =
  (* This is loosy parsing... *)
  let re = Str.regexp "\\(.\\|\n\\)*icy-metaint:\\([0-9]+\\)" in
    try
      if Str.string_match re h 0 then
        int_of_string (Str.matched_group 2 h)
      else
        -1
    with
      | _ -> -1

let read_metadata socket =
  let size =
    let buf = " " in
    let s = Unix.read socket buf 0 1 in
      assert (s=1) ;
      int_of_char buf.[0]
  in
  let size = 16*size in
  let chunk =
    let buf = String.create size in
    let rec read pos =
      if pos=size then buf else
        let p = Unix.read socket buf pos (size-pos) in
          assert (p>0) ;
          read (pos+p)
    in
      read 0
  in
  let h = Hashtbl.create 10 in
  let rec parse s =
    try
      let mid = String.index s '=' in
      let close = String.index s ';' in
      let key = Configure.recode_tag (String.sub s 0 mid) in
      let value = Configure.recode_tag (String.sub s (mid+2) (close-mid-3)) in
      let key =
        match key with
          | "StreamTitle" -> "title"
          | "StreamUrl" -> "url"
          | _ -> key
      in
        Hashtbl.add h key value ;
        parse (String.sub s (close+1) ((String.length s)-close-1))
    with _ -> ()
  in
    if chunk="" then None else Some (parse chunk ; h)

class http ~autostart url samplefreq =
  (* Url format: <http://host/mount> or <http://host:port/mount> *)
  let host,port,mount = Http.parse_url url in
  let get = Printf.sprintf "GET %s HTTP/1.0\r\nIcy-MetaData:1\r\n\r\n" mount in
  let get_length = String.length get in
object (self)
  inherit source

  method stype = Fallible

  val abg_max_len = 1000*Mixer.Buffer.size
  val lock = Mutex.create ()
  val abg = Mixer.Generator.create ()

  val mutable metadata = None

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
          let buf = Mad.decode_frame dec in
            (* Update buffer information *)
            Mutex.lock lock ;
            locked := true ;
            Mixer.Generator.feed abg
              { Mixer.channels = Mad.wav_output_channels ;
                Mixer.sample_freq = samplefreq ;
                Mixer.sample_size = Mad.wav_output_sample_size ;
                Mixer.big_endian = Mad.wav_output_big_endian ;
                Mixer.signed = Mad.wav_output_signed }
              buf ;
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
            if e = Mad.End_of_stream then false else true
    in
      if again && not sleeping then self#feeding ~newstream:false dec else begin
        self#log 3 "Feeding stopped. Closing decoder." ;
        Mad.close dec ;
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
        if Mixer.Generator.length abg > 0 then begin
          begin match metadata with
            | None -> ()
            | Some m ->
                Mixer.Buffer.set_metadata ab 0 m ;
                metadata <- None ;
                self#log 3
                  (Dtools.Log.f "Metadata %S put in buffer."
                     (try Hashtbl.find m "title" with _ -> "?"))
          end ;
          Mixer.Buffer.fill ab abg ;
        end else begin
          Mixer.Buffer.blankify ab p (Mixer.Buffer.size - p) ;
          Mixer.Buffer.add_break ab Mixer.Buffer.size
        end ;
        Mutex.unlock lock

  val mutable relaying = autostart

  (* Called when there's no decoding process, in order to create one. *)
  method connect =
    self#log 4
      (Dtools.Log.f "Trying to connect to http://%s:%d%s" host port mount) ;
    let socket = socket PF_INET SOCK_STREAM 0 in try
      connect socket (ADDR_INET((gethostbyname host).h_addr_list.(0), port)) ;
      try
        if Unix.write socket get 0 get_length < get_length then raise Internal ;
        let header = String.create 4096 in
        let h = Unix.read socket header 0 4096 in
          if h < 12 then failwith "No header to read" else
            ( self#log 4 (String.sub header 0 12) ;
              if not (Http.ok header) then raise Internal ) ;
	  let metaint = metaint_of_header header in
          self#log 4
            (Dtools.Log.f "Got header from http://%s:%d%s (metaint=%d)"
               host port mount metaint) ;
	  let bytes_read = ref 0 in
	  let rec reader len =
            let close () =
              try
                shutdown socket SHUTDOWN_ALL ;
                close socket ;
                "",0
              with _ -> "",0
            in
              if !bytes_read=metaint then begin
                bytes_read := 0 ;
                if
                  try metadata <- read_metadata socket ; true with _ -> false
                then
                  reader len
                else
                  close ()
              end else
                let len =
                  if metaint = -1 then len else
                    (* Don't read the beginning of metadatas. *)
                    min len (metaint - !bytes_read)
                in
                let b = String.create len in
                try
                  if not relaying then failwith "Relaying disabled" ;
                  let r = Unix.read socket b 0 len in
                    if metaint>0 && r>0 then bytes_read := !bytes_read + r ;
                    if r <= 0 then close () else b,r
                with
                  | e -> self#log 2 (Dtools.Log.f "Read error %s"
                                       (Printexc.to_string e)) ;
                         close ()
          in
          let dec = Mad.openstream reader in
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
    if relaying then self#connect ;
    if sleeping then
      Mutex.unlock polling
    else
      ( Thread.delay 1. ; self#poll )

  val mutable ns = []

  method wake_up _ =
    (* Wait for the polling thread to return *)
    Mutex.lock polling ;
    sleeping <- false ;
    ignore (Tutils.create (fun () -> self#poll) () "http polling") ;
    if ns = [] then
      ns <- Server.register [self#id] "input.http.mp3" ;
    self#set_id (Server.to_string ns) ;
    Server.add ~ns "start" ~usage:"start" (fun _ -> relaying <- true ; "Done") ;
    Server.add ~ns "stop" ~usage:"stop" (fun _ -> relaying <- false ; "Done")

  method sleep =
    sleeping <- true

end

let _ =
    Lang.add_operator "input.http.mp3"
      ~descr:("Forwards the given MP3 http stream. The relay can be "^
              "paused/resumed using the start/stop telnet commands.")
      [ "", Lang.string_t, None,
        Some "URL of an http mp3 stream (default port is 8000)." ;
        "autostart", Lang.bool_t, Some (Lang.bool true),
        Some "Initially start relaying or not." ;
        "samplefreq", Lang.int_t, Some (Lang.int 44100),
        Some "Samplefreq of the input stream." ]
      (fun p ->
         let freq = Lang.to_int (List.assoc "samplefreq" p) in
         let autostart = Lang.to_bool (List.assoc "autostart" p) in
         let url = Lang.to_string (List.assoc "" p) in
           ((new http ~autostart url freq):>source))
