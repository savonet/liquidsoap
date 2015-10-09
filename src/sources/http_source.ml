(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2015 Savonet team

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
exception Read_error

(** Error translator *)
let error_translator e =
   match e with
     | Internal -> Some "Internal http error."
     | Read_error ->
         Some "Error while reading http stream."
     | _ -> None

let () = Utils.register_error_translator error_translator

(** Types for playlist handling *)
type playlist_mode =  Random | First | Randomize | Normal

(** Utility for reading icy metadata *)
let read_metadata () = let old_chunk = ref "" in fun socket ->
  let size =
    let buf = " " in
    let s = Unix.read socket buf 0 1 in
      if s<>1 then raise Read_error ;
      int_of_char buf.[0]
  in
  let size = 16*size in
  let chunk =
    let buf = Bytes.create size in
    let rec read pos =
      if pos=size then buf else
        let p = Unix.read socket buf pos (size-pos) in
          if p<=0 then raise Read_error ;
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
    if chunk = "" then
      None
    else if chunk = !old_chunk then
      None
    else begin
      old_chunk := chunk ;
      parse chunk ;
      Some h
    end

let read_line socket =
  let ans = ref "" in
  let c = Bytes.create 1 in
    if Unix.read socket c 0 1 <> 1 then raise Read_error ;
    while c <> "\n" do
      ans := !ans ^ c;
      if Unix.read socket c 0 1 <> 1 then raise Read_error
    done;
    String.sub !ans 0 (String.length !ans - 1)

let read_chunk socket =
  let n = read_line socket in
  let n = Scanf.sscanf n "%x" (fun n -> n) in
  let ans = ref "" in
    while String.length !ans <> n do
      let buf = Bytes.create (n - String.length !ans) in
      let r = Unix.read socket buf 0 (n - String.length !ans) in
        ans := !ans ^ (String.sub buf 0 r)
    done;
    !ans

let read_stream socket chunked metaint insert_metadata =
  let read_metadata = read_metadata () in
  let chunkbuf = ref "" in
  let read buf offs len =
    if chunked then begin
      if String.length !chunkbuf = 0 then chunkbuf := read_chunk socket;
      let n = min len (String.length !chunkbuf) in
        String.blit !chunkbuf 0 buf offs n;
        chunkbuf := String.sub !chunkbuf n (String.length !chunkbuf - n);
        n
    end else
      Unix.read socket buf 0 len
  in
    match metaint with
      | None ->
          fun len ->
            let b = Bytes.create len in
            let r = read b 0 len in
              if r < 0 then "",0 else b,r
      | Some metaint ->
          let readcnt = ref 0 in
            fun len ->
              let len = min len (metaint - !readcnt) in
              let b = Bytes.create len in
              let r = read b 0 len in
                if r < 0 then "",0 else begin
                  readcnt := !readcnt + r;
                  if !readcnt = metaint then begin
                    readcnt := 0;
                    match read_metadata socket with
                      | Some m -> insert_metadata m
                      | None -> ()
                  end ;
                  b,r
                end

(** HTTP input *)

let url_expr = Str.regexp "^http://\\([^/]+\\)\\(/.*\\)?$"
let host_expr = Str.regexp "^\\([^:]+\\):\\([0-9]+\\)$"
let auth_split_expr = Str.regexp "^\\([^@]+\\)@\\(.+\\)$"

let parse_url url =
  let host,mount =
    if Str.string_match url_expr url 0 then
      (Str.matched_group 1 url),
      (try Str.matched_group 2 url with Not_found -> "/")
    else
      failwith (Printf.sprintf "Invalid URL %S!" url)
  in
  let auth,host =
    if Str.string_match auth_split_expr host 0 then
      (Str.matched_group 1 host),
      (Str.matched_group 2 host)
    else
      "",host
  in
    if Str.string_match host_expr host 0 then
      (Str.matched_group 1 host),
      (int_of_string (Str.matched_group 2 host)),
      mount,
      auth
    else
      host,80,mount,auth

module G = Generator
module Generator = Generator.From_audio_video_plus
module Generated = Generated.Make(Generator)

(* Used to handle redirections. *)
exception Redirection of string

class http ~kind
        ~playlist_mode ~poll_delay ~track_on_meta ?(force_mime=None)
        ~bind_address ~autostart ~bufferize ~max ~timeout
        ~debug ~on_connect ~on_disconnect ?(logfile=None)
        ~user_agent url =
  let max_ticks = Frame.master_of_seconds (Pervasives.max max bufferize) in
  (* We need a temporary log until the source has an ID. *)
  let log_ref = ref (fun _ -> ()) in
  let log = (fun x -> !log_ref x) in
object (self)

  inherit  Source.source ~name:"http" kind as super
  inherit
    Generated.source
      (Generator.create ~log ~kind ~overfull:(`Drop_old max_ticks) `Undefined)
      ~empty_on_abort:false ~bufferize

  method stype = Source.Fallible

  (** POSIX sucks. *)
  val mutable socket = None
  (* Mutex to change the socket's state (open, close) *)
  val mutable socket_m = Mutex.create()

  val mutable url = url

  (** [kill_polling] is for requesting that the feeding thread stops;
    * it is called on #sleep. *)
  val mutable kill_polling = None

  (** [wait_polling] is to make sure that the thread did stop;
    * it is only called in #wake_up before creating a new thread,
    * so that #sleep is instantaneous. *)
  val mutable wait_polling = None

  (** Log file for the timestamps of read events. *)
  val mutable logf = None

  val mutable relaying = autostart
  val mutable playlist_mode = playlist_mode

  initializer
    ns_kind <- "input.http" ;
    self#register_command "start" ~usage:"start"
                          ~descr:"Start the source, if needed."
      (fun _ -> relaying <- true ; "Done") ;
    self#register_command "stop" ~usage:"stop"
                          ~descr:"Stop the source if streaming."
      (fun _ -> relaying <- false ; "Done") ;
    self#register_command "url" ~usage:"url [url]"
      ~descr:"Get or set the stream's HTTP URL. \
              Setting a new URL will not affect an ongoing connection."
      (fun u ->
        if u = "" then url else
          try ignore (parse_url u) ; url <- u ; "Done" with
            | Failure _ -> "Invalid URL") ;
    self#register_command "status" ~usage:"status"
      ~descr:"Return the current status of the source, \
              either \"stopped\" (the source isn't trying to relay \
              the HTTP stream), \
              \"polling\" (attempting to connect to the HTTP stream) \
              or \"connected <url>\" (connected to <url>, buffering or \
              playing back the stream)."
       (Tutils.mutexify socket_m (fun _ ->
         match socket with
           | Some (_,_,url) -> "connected " ^ url
           | None -> if relaying then "polling" else "stopped")) ;
    self#register_command "buffer_length" ~usage:"buffer_length"
                          ~descr:"Get the buffer's length, in seconds."
      (fun _ -> Printf.sprintf "%.2f" (Frame.seconds_of_audio self#length))

  (* Insert metadata *)
  method insert_metadata m =
    self#log#f 3
      "New metadata chunk: %s -- %s."
      (try Hashtbl.find m "artist" with _ -> "?")
      (try Hashtbl.find m "title" with _ -> "?") ;
    Generator.add_metadata generator m ;
    if track_on_meta then Generator.add_break ~sync:`Ignore generator

  method feeding should_stop create_decoder =
    let read =
      let log = self#log#f 4 "%s" in
      (* Socket can't be closed while waiting on it. *)
      (fun len ->
        let socket = Tutils.mutexify socket_m (fun () ->
          socket) ()
        in
        match socket with
          | None -> "",0
          | Some (socket,read,_) ->
              begin
               try
                Tutils.wait_for ~log `Read socket timeout;
                read len
               with e -> self#log#f 2 "Error while reading from socket: \
                            %s" (Printexc.to_string e);
                         self#disconnect_no_lock;
                         "",0
               end)
    in
    let read =
      match logf with
        | None -> read
        | Some f ->
            let t0 = Unix.gettimeofday () in
              fun len ->
                let ret = read len in
                let time = (Unix.gettimeofday () -. t0) /. 60. in
                  Printf.fprintf f "%f %d\n%!" time self#length ;
                  ret
    in
    let input =
      { Decoder.
         read = read ;
         tell = None;
         length = None;
         lseek = None }
    in
      try
        let decoder = create_decoder input in
        while true do
          if should_fail then failwith "end of track" ;
          if should_stop () || (not relaying) then
            failwith "source stopped" ;
          decoder.Decoder.decode generator
        done
      with
        | e ->
            if debug then raise e ;
            (* Feeding has stopped: adding a break here. *)
            Generator.add_break ~sync:`Drop generator ;
            begin match e with
              | Failure s ->
                  self#log#f 2 "Feeding stopped: %s" s
              | G.Incorrect_stream_type ->
                self#log#f 2 "Feeding stopped: the decoded stream was not of \
                              the right type. The typical situation is when \
                              you expect a stereo stream whereas the http \
                              stream is mono (in this case the situation can \
                              easily be solved by using the audio_to_stereo \
                              operator to convert the stream to a stereo one)."
              | e ->
                  self#log#f 2 "Feeding stopped: %s" (Printexc.to_string e)
            end ;
            begin match logf with
              | Some f -> close_out f ; logf <- None
              | None -> ()
            end ;
            self#disconnect

  method private disconnect_no_lock =
    Utils.maydo (fun (s,_,_) ->
     try
      Http.disconnect s;
      on_disconnect ()
     with _ -> ()) socket;
    socket <- None

  method disconnect =
    Tutils.mutexify socket_m (fun () ->
      self#disconnect_no_lock) ()

  (* Called when there's no decoding process, in order to create one. *)
  method connect poll_should_stop url =
    let host,port,mount,auth = parse_url url in
    let req =
      Printf.sprintf
        "GET %s HTTP/1.0\r\nHost: %s:%d\r\n"
        mount host port
    in
    let auth =
      match auth with
        | "" -> ""
        | _ -> "Authorization: Basic " ^ (Utils.encode64 auth) ^ "\r\n"
    in
    let request =
      Printf.sprintf
        "%sUser-Agent: %s\r\n%sIcy-MetaData:1\r\n\r\n"
        req user_agent auth
    in
    try
      let (_, status, status_msg), fields =
        Tutils.mutexify socket_m (fun () ->
          if socket <> None then
            failwith "Cannot connect while already connected..";
          self#log#f 4 "Connecting to <http://%s:%d%s>..." host port mount ;
          let s = Http.connect ?bind_address host port in
          let log = self#log#f 4 "%s" in
          let (_, fields as ret) =
            Http.request ~log ~timeout s request
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
          if chunked then
            self#log#f 4 "Chunked HTTP/1.1 transfer" ;
          (* read_stream has a state, so we must create it here.. *)
          let read =
            read_stream s chunked metaint self#insert_metadata
          in
          socket <- Some (s,read,url);
          ret) ()
      in
      let content_type =
        match force_mime with
          | Some s -> s
          | None ->
             let content_type =
               try List.assoc "content-type" fields with Not_found -> "unknown"
             in
             (* Remove modifiers from content type. *)
             try
               let sub = Pcre.exec ~pat:"^([^;]+);.*$" content_type in
               Pcre.get_substring sub 1
             with
               | Not_found -> content_type
      in
      self#log#f 4 "Content-type %S." content_type ;
      if status = 301 || status = 302 || 
         status = 303 || status = 307
      then begin
        let location =
          try
            List.assoc "location" fields
          with
            | Not_found -> raise Internal
        in
        let location =
          if location <> "" && location.[0] = '/' then
            Printf.sprintf "http://%s:%d%s" host port location
          else
            location
        in
        self#log#f 4 "Redirected to %s" location;
        raise (Redirection location)
      end ;
      if status <> 200 then begin
        self#log#f 4 "Could not get file: %s" status_msg;
        raise Internal
      end ;
      on_connect fields ;
      let play_track (m,uri) =
        if not (poll_should_stop ()) then
          let metas = Hashtbl.create 2 in
          List.iter (fun (a,b) -> Hashtbl.add metas a b) m;
          self#insert_metadata metas;
          self#disconnect;
          self#connect poll_should_stop uri
      in
      let randomize playlist =
        let aplay = Array.of_list playlist in
         Utils.randomize aplay;
         Array.to_list aplay
      in
      let playlist_process playlist =
        try
          match playlist_mode with
            | Random ->  play_track (List.hd (randomize playlist))
            | First -> play_track (List.hd playlist)
            | Randomize -> List.iter play_track (randomize playlist)
            | Normal -> List.iter play_track playlist
        with
          | Failure _ -> raise Not_found
      in
      let test_playlist parser =
        let playlist =
          Tutils.mutexify socket_m (fun () ->
            match socket with
            | None -> failwith "not connected!"
            | Some (s,_,_) ->
              let content = Http.read ~timeout s None in
              let playlist = parser content in
              match playlist with
              | [] -> raise Not_found
              | _ -> playlist) ()
        in
        playlist_process playlist
      in
        try
          self#log#f 4
            "Trying playlist parser for mime %s" content_type ;
          match Playlist_parser.parsers#get content_type with
            | None -> raise Not_found
            | Some plugin ->
              let pwd = Http.dirname url in
              test_playlist (plugin.Playlist_parser.parser ~pwd)
        with
          | Not_found ->
              (* Trying playlist auto parsing in case
               * of content type text/plain *)
              if content_type = "text/plain" then begin
                try
                  test_playlist
                    (fun x -> snd (Playlist_parser.search_valid ~pwd:(Http.dirname url) x))
                with
                  | Not_found -> ()
              end else begin
                Generator.set_mode generator `Undefined ;
                let dec =
                  match
                    Decoder.get_stream_decoder content_type kind
                  with
                    | Some d -> d
                    | None -> failwith "Unknown format!"
                in
                begin match logfile with
                  | Some f ->
                      begin try
                        logf <-
                          Some (open_out_bin (Utils.home_unrelate f))
                      with e ->
                        self#log#f 2
                          "Could not open log file: %s"
                          (Printexc.to_string e)
                      end
                  | None -> ()
                end ;
                self#log#f 3 "Decoding..." ;
                Generator.set_rewrite_metadata generator
                  (fun m -> Hashtbl.add m "source_url" url ; m) ;
                self#feeding
                  poll_should_stop dec
            end
    with
      | Redirection location ->
          self#disconnect;
          self#connect poll_should_stop location
      | Http.Error e ->
          self#disconnect;
          self#log#f 4 "Connection failed: %s!" (Http.string_of_error e) ;
          if debug then raise (Http.Error e)
      | e ->
          self#disconnect;
          self#log#f 4 "Connection failed: %s" (Printexc.to_string e) ;
          if debug then raise e

  (* Take care of (re)starting the decoding *)
  method poll (should_stop,has_stopped) =
    (* Try to read the stream *)
    if relaying then
      self#connect should_stop url ;
    if should_stop () then
      has_stopped ()
    else begin
      Thread.delay poll_delay ;
      self#poll (should_stop,has_stopped)
    end

  method wake_up act =
    super#wake_up act ;
    (* Now we can create the log function *)
    log_ref := (fun s -> self#log#f 3 "%s" s) ;
    (* Wait for the old polling thread to return, then create a new one. *)
    assert (kill_polling = None) ;
    begin match wait_polling with
      | None -> ()
      | Some f -> f () ; wait_polling <- None
    end ;
    begin
      let kill,wait = Tutils.stoppable_thread self#poll "http polling" in
        kill_polling <- Some kill ;
        wait_polling <- Some wait
    end

  method sleep =
    (Utils.get_some kill_polling) () ;
    kill_polling <- None

end

let () =
  Lang.add_operator "input.http"
    ~kind:(Lang.Unconstrained (Lang.univ_t 1))
    ~category:Lang.Input
    ~descr:"Forwards the given http stream. The relay can be \
            paused/resumed using the start/stop telnet commands."
    [ "autostart", Lang.bool_t, Some (Lang.bool true),
      Some "Initially start relaying or not." ;

      "bind_address", Lang.string_t, Some (Lang.string ""),
      Some "Address to bind on the local machine. \
            This option can be useful if \
            your machine is bound to multiple IPs. \
            Empty means no bind address." ;

      "buffer", Lang.float_t, Some (Lang.float 2.),
      Some "Duration of the pre-buffered data." ;

      "timeout", Lang.float_t, Some (Lang.float 30.),
      Some "Timeout for source connectionn.";

      "on_connect",
      Lang.fun_t [false,"",Lang.metadata_t] Lang.unit_t,
      Some (Lang.val_cst_fun ["",Lang.metadata_t,None] Lang.unit),
      Some "Function to execute when a source is connected. \
            Its receives the list of headers, of the form: \
            (<label>,<value>). All labels are lowercase.";

      "on_disconnect",Lang.fun_t [] Lang.unit_t,
      Some (Lang.val_cst_fun [] Lang.unit),
      Some "Function to excecute when a source is disconnected";

      "new_track_on_metadata", Lang.bool_t, Some (Lang.bool true),
      Some "Treat new metadata as new track." ;

      "force_mime", Lang.string_t, Some (Lang.string ""),
      Some "Force mime data type. Not used if empty." ;

      "playlist_mode", Lang.string_t, Some (Lang.string "normal"),
      Some "Valid modes are \"normal\", \"random\", \"randomize\" \
            and \"first\". The first ones have the same meaning as for \
            the mode parameter of the playlist operator. The last one \
            discards all entries but the first one." ;

      "poll_delay", Lang.float_t, Some (Lang.float 2.),
      Some "Polling delay when trying to connect to the stream." ;

      "max", Lang.float_t, Some (Lang.float 10.),
      Some "Maximum duration of the buffered data." ;

      "logfile", Lang.string_t, Some (Lang.string ""),
      Some "Log buffer status to file, for debugging purpose. \
            Disabled if empty." ;

      "debug", Lang.bool_t, Some (Lang.bool false),
      Some "Run in debugging mode, not catching some exceptions." ;

      "user_agent", Lang.string_t,
      Some (Lang.string Http.user_agent),
      Some "User agent." ;

      "", Lang.string_t, None,
      Some "URL of an http stream (default port is 80)." ]
    (fun p kind ->
       let playlist_mode =
         let s = List.assoc "playlist_mode" p in
           match Lang.to_string s with
             | "random" -> Random
             | "first" -> First
             | "randomize" -> Randomize
             | "normal" -> Normal
             | _ ->
                 raise
                   (Lang.Invalid_value
                      (s,
                       "valid values are 'random', 'randomize', \
                        'normal' and 'first'"))
       in
       let url = Lang.to_string (List.assoc "" p) in
       let () =
         try ignore (parse_url url) with
           | Failure _ ->
               raise (Lang.Invalid_value (List.assoc "" p, "invalid URL"))
       in
       let autostart = Lang.to_bool (List.assoc "autostart" p) in
       let bind_address = Lang.to_string (List.assoc "bind_address" p) in
       let user_agent = Lang.to_string (List.assoc "user_agent" p) in
       let timeout = Lang.to_float (List.assoc "timeout" p) in
       let track_on_meta =
         Lang.to_bool (List.assoc "new_track_on_metadata" p)
       in
       let debug = Lang.to_bool (List.assoc "debug" p) in
       let logfile =
         match Lang.to_string (List.assoc "logfile" p) with
           | "" -> None
           | s -> Some s
       in
       let bind_address =
         match bind_address with
           | "" -> None
           | s -> Some s
       in
       let force_mime =
         match Lang.to_string (List.assoc "force_mime" p) with
           | "" -> None
           | s  -> Some s
       in
       let bufferize = Lang.to_float (List.assoc "buffer" p) in
       let max = Lang.to_float (List.assoc "max" p) in
       if bufferize >= max then
         raise (Lang.Invalid_value
                  (List.assoc "max" p,
                   "Maximum buffering inferior to pre-buffered data"));
       let on_connect l =
         let l =
           List.map
            (fun (x,y) -> Lang.product (Lang.string x) (Lang.string y))
            l
         in
         let arg =
           Lang.list ~t:(Lang.product_t Lang.string_t Lang.string_t) l
         in
         ignore
           (Lang.apply ~t:Lang.unit_t (List.assoc "on_connect" p) ["",arg])
       in
       let on_disconnect () =
         ignore
           (Lang.apply ~t:Lang.unit_t (List.assoc "on_disconnect" p) [])
       in
       let poll_delay = Lang.to_float (List.assoc "poll_delay" p) in
         ((new http ~kind ~playlist_mode ~autostart ~track_on_meta
                    ~force_mime ~bind_address ~poll_delay ~timeout
                    ~on_connect ~on_disconnect ~bufferize ~max 
                    ~debug ~logfile ~user_agent url)
            :> Source.source))
