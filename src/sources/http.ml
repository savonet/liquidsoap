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

exception Internal

(** Decoders might raise this exception to play a playlist. *)
exception Playlist of (string list)

(** Interface with stream decoders.
  * We can't use Decoder since it is designed for files, and estimates
  * the remaining number of frames. *)

type sink = {
  read : int -> string ;
  put : int -> float array array -> unit ;
  insert_metadata : Frame.metadata -> unit ;
  close : unit -> unit
}

let stream_decoders : (sink -> unit) Plug.plug =
  Plug.create ~doc:"Methods for decoding audio streams." "stream_formats"

(** Utilities for reading icy metadata *)

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

let read_line socket =
  let ans = ref "" in
  let c = String.create 1 in
    assert (Unix.read socket c 0 1 = 1);
    while c <> "\n" do
      ans := !ans ^ c;
      assert (Unix.read socket c 0 1 = 1);
    done;
    String.sub !ans 0 (String.length !ans - 1)

let read_chunk socket =
  let n = read_line socket in
  let n = Scanf.sscanf n "%x" (fun n -> n) in
  let ans = ref "" in
    while String.length !ans <> n do
      let buf = String.create (n - String.length !ans) in
      let r = Unix.read socket buf 0 (n - String.length !ans) in
        ans := !ans ^ (String.sub buf 0 r)
    done;
    !ans

let read_stream socket chunked metaint insert_metadata =
  let chunkbuf = ref "" in
  let read buf offs len =
    if chunked then
      (
        if String.length !chunkbuf = 0 then chunkbuf := read_chunk socket;
        let n = min len (String.length !chunkbuf) in
          String.blit !chunkbuf 0 buf offs n;
          chunkbuf := String.sub !chunkbuf n (String.length !chunkbuf - n);
          n
      )
    else
      Unix.read socket buf 0 len
  in
    match metaint with
      | None ->
          fun len ->
            let b = String.create len in
            let r = read b 0 len in
              if r < 0 then "" else String.sub b 0 r
      | Some metaint ->
          let readcnt = ref 0 in
            fun len ->
              let len = min len (metaint - !readcnt) in
              let b = String.create len in
              let r = read b 0 len in
                if r < 0 then "" else begin
                  readcnt := !readcnt + r;
                  if !readcnt = metaint then begin
                    readcnt := 0;
                    match read_metadata socket with
                      | Some m -> insert_metadata m
                      | None -> ()
                  end ;
                  String.sub b 0 r
                end

(** Generic http input *)

let url_expr = Str.regexp "^http://\\([^/]+\\)\\(/.*\\)?$"
let host_expr = Str.regexp "^\\([^:]+\\):\\([0-9]+\\)$"

let parse_url url =
  let host,mount =
    if Str.string_match url_expr url 0 then
      (Str.matched_group 1 url),
      (try Str.matched_group 2 url with Not_found -> "/")
    else
      failwith (Printf.sprintf "Invalid URL %S!" url)
  in
    if Str.string_match host_expr host 0 then
      (Str.matched_group 1 host),
      (int_of_string (Str.matched_group 2 host)),
      mount
    else
      host,80,mount

let ok header = Pcre.pmatch ~pat:".*200 OK" header

module Generator = Float_pcm.Generator
module Generated = Generated.From_Float_pcm_Generator

class http ~autostart ~bufferize ~max url =
  let abg_max_len = Fmt.samples_of_seconds max in
object (self)
  inherit Source.source
  inherit Generated.source
            (Generator.create ())
            ~empty_on_abort:false ~bufferize

  method stype = Source.Fallible

  val polling = Mutex.create ()
  val mutable sleeping = true
  val mutable connected = false
  val mutable relaying = autostart

  val mutable host = ""
  val mutable port = 0
  val mutable mount = ""
  val mutable request = ""

  method set_url url =
    let h,p,m = parse_url url in
    let req =
      Printf.sprintf
        "GET %s HTTP/1.0\r\nHost: %s:%d\r\n"
        m h p
    in
    let req =
      Printf.sprintf
        "%sUser-Agent: liquidsoap/%s (%s; ocaml %s)\r\nIcy-MetaData:1\r\n\r\n"
        req Configure.version Sys.os_type Sys.ocaml_version
    in
      host <- h;
      port <- p;
      mount <- m;
      request <- req

  initializer self#set_url url

  (* Feed the buffer generator *)
  method feeding ?(newstream=true) dec socket chunked metaint =
    connected <- true ;
    let close () =
      Unix.shutdown socket Unix.SHUTDOWN_ALL ;
      Unix.close socket
    in
    let insert_metadata m =
      self#log 3 (Dtools.Log.f "New metadata chunk \"%s -- %s\""
                    (try Hashtbl.find m "artist" with _ -> "?")
                    (try Hashtbl.find m "title" with _ -> "?")) ;
      metadata <- m ;
      metadata_pos <- Fmt.ticks_of_samples (Generator.length abg)
    in
    let put sample_freq data =
      if not relaying then failwith "relaying stopped" ;
      Mutex.lock lock ;
      (* TODO There must be two ways of handling overfull generator:
       * (1) when streaming, one should just stop the decoder for a while;
       * (2) when not streaming, one should throw some data.
       * Doing 1 instead of 2 can lead to deconnections.
       * Doing 2 instead of 1 leads to ugly sound.
       * The problem is:
       * we don't know when the source will really be asked for data... *)
      while Generator.length abg >= abg_max_len do
        Mutex.unlock lock ;
        Thread.delay (max /. 3.) ;
        Mutex.lock lock
      done ;
      Generator.feed abg ~sample_freq data ;
      Mutex.unlock lock
    in
    let read = read_stream socket chunked metaint insert_metadata in
    let sink =
      { put = put ; read = read ;
        insert_metadata = insert_metadata ; close = close }
    in
      try dec sink with
        | Playlist urls ->
            if urls = [] then begin
              self#log 2 "Received empty playlist";
              raise Internal (* TODO: better error *)
            end ;
            self#log 3
              (Printf.sprintf "Switching to playlist url: %s" (List.hd urls)) ;
            (* TODO: connect to every url? *)
            self#set_url (List.hd urls);
            self#connect
        | e -> self#log 2 (Printf.sprintf "Feeding stopped: %s"
                             (Printexc.to_string e))

  (* Called when there's no decoding process, in order to create one. *)
  method connect =
    self#log 4
      (Dtools.Log.f "Connecting to <http://%s:%d%s>..." host port mount) ;
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in try
      Unix.connect
        socket
        (Unix.ADDR_INET((Unix.gethostbyname host).Unix.h_addr_list.(0),port)) ;
      try
        if Unix.write socket request 0 (String.length request) < (String.length request) then
          raise Internal ;
        let header =
          (* We read until we see \r\n\r\n *)
          let ans = ref "" in
          let n = ref 0 in
          let loop = ref true in
          let was_n = ref false in
          let c = String.create 1 in
            while !loop && !n < 4096 do
              let h = Unix.read socket c 0 1 in
                if h < 1 then
                  loop := false
                else
                  (
                    ans := !ans ^ c;
                    if c = "\n" then
                      (if !was_n then loop := false else was_n := true)
                    else if c <> "\r" then
                      was_n := false
                  );
                incr n
            done;
            !ans
        in
        let fields =
          let ans = ref [] in
            List.iter
              (fun line ->
                 try
                   let sub = Pcre.exec ~pat:"([^:]*):\\s*(.*)" line in
                   let key = String.lowercase (Pcre.get_substring sub 1) in
                   let value = Pcre.get_substring sub 2 in
                     ans := (key,value) :: ! ans
                 with
                   | Not_found -> ())
              (Pcre.split ~pat:"\r\n" header) ;
            !ans
        in
        let content_type =
          try List.assoc "content-type" fields with Not_found -> "unknown"
        in
        let metaint =
          try
            Some (int_of_string (List.assoc "icy-metaint" fields))
          with _ -> None
        in
        let chunked =
          try
            List.assoc "transfer-encoding" fields = "chunked"
          with _ -> false
        in
          if not (ok header) then raise Internal ;
          self#log 4 (Printf.sprintf "Content-type \"%s\"." content_type) ;
          if chunked then self#log 4 ("Chunked HTTP/1.1 transfer");
          let dec =
            match
              stream_decoders#get content_type
            with
              | Some d -> d
              | None -> failwith "Unknown format!"
          in
            self#log 3 "Decoding..." ;
            self#feeding dec socket chunked metaint
      with
      | e ->
          self#log 4
            (Dtools.Log.f "Connection failed: %s" (Printexc.to_string e)) ;
          Unix.shutdown socket Unix.SHUTDOWN_ALL ;
          Unix.close socket
    with
    | e ->
        self#log 4
          (Dtools.Log.f "Connection failed: %s" (Printexc.to_string e)) ;
        Unix.close socket

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
      ns <- Server.register [self#id] "input.http" ;
    self#set_id (Server.to_string ns) ;
    Server.add ~ns "start" ~usage:"start" (fun _ -> relaying <- true ; "Done") ;
    Server.add ~ns "stop" ~usage:"stop" (fun _ -> relaying <- false ; "Done")

  method sleep =
    sleeping <- true

end

let () =
    Lang.add_operator "input.http"
      ~category:Lang.Input
      ~descr:("Forwards the given http stream. The relay can be "^
              "paused/resumed using the start/stop telnet commands.")
      [ "autostart", Lang.bool_t, Some (Lang.bool true),
        Some "Initially start relaying or not." ;
        "buffer", Lang.float_t, Some (Lang.float 2.),
        Some "Duration of the pre-buffered data." ;
        "max", Lang.float_t, Some (Lang.float 10.),
        Some "Maximum duration of the buffered data." ;
        "", Lang.string_t, None,
        Some "URL of an http stream (default port is 80)." ]
      (fun p ->
         let url = Lang.to_string (List.assoc "" p) in
         let autostart = Lang.to_bool (List.assoc "autostart" p) in
         let bufferize = Lang.to_float (List.assoc "buffer" p) in
         let max = Lang.to_float (List.assoc "max" p) in
           ((new http ~autostart ~bufferize ~max url):>Source.source))
