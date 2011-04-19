(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

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

open Unix

module Generator = Generator.From_audio_video_plus
module Generated = Generated.Make(Generator)

exception Disconnected
exception Stopped

(** Error translator *)
let error_translator e =
   match e with
     | Disconnected ->
         raise (Utils.Translation "Source client disconnected")
     | Stopped ->
         raise (Utils.Translation "Source stopped")
     | _ -> ()

let () = Utils.register_error_translator error_translator

(* {1 Input handling} *)

class http_input_server ~kind ~dumpfile ~logfile
                        ~bufferize ~max ~icy ~port
                        ~meta_charset ~icy_charset
                        ~mountpoint ~on_connect ~on_disconnect
                        ~login ~debug p =
  let max_ticks = Frame.master_of_seconds max in
  (* We need a temporary log until
   * the source has an id *)
  let log_ref = ref (fun _ -> ()) in
  let log = (fun x -> !log_ref x) in
object (self)
  inherit Source.source ~name:"harbor" kind
  inherit Generated.source
            (Generator.create
               ~log ~kind ~overfull:(`Drop_old max_ticks) `Undefined)
            ~empty_on_abort:false ~bufferize as generated

  val mutable relay_socket = None
  val relay_m = Mutex.create ()
  val mutable stopped = false
  val mutable create_decoder = fun _ -> assert false
  val mutable mime_type = None

  (** [kill_polling] is for requesting that the feeding thread stops;
    * it is called on #disconnect. *)
  val mutable kill_polling = None

  (** [wait_polling] is to make sure that the thread did stop;
    * it is only called in #relay before creating a new thread,
    * so that #disconnect is instantaneous. *)
  val mutable wait_polling = None

  val mutable dump = None
  val mutable logf = None

  initializer 
    ns_kind <- "input.harbor" ;
    let stop =
      Tutils.mutexify relay_m
        (fun _ ->
          if relay_socket <> None then 
            (self#disconnect ; "Done")
          else 
            "No source client connected")
    in
    self#register_command
      "stop" ~descr:"Stop current source client, if connected." stop ;
    self#register_command
      "kick" ~descr:"Kick current source client, if connected." stop ;
    self#register_command
      "status" ~descr:"Display current status."
      (Tutils.mutexify relay_m 
       (fun _ ->
         match relay_socket with
           | Some s ->
               Printf.sprintf "source client connected from %s"
                  (Utils.name_of_sockaddr ~rev_dns:Harbor.conf_revdns#get
                                         (Unix.getpeername s))
           | None ->
               "no source client connected")) ;
    self#register_command 
               "buffer_length" ~usage:"buffer_length"
               ~descr:"Get the buffer's length, in seconds."
       (fun _ -> Printf.sprintf "%.2f"
             (Frame.seconds_of_audio self#length))

  method login : string*(string -> string -> bool) = login

  method stype = Source.Fallible

  method icy_charset = icy_charset
  method meta_charset = meta_charset

  (* Insert metadata *)
  method insert_metadata m =
    (* Metadata may contain only the "song" value
     * or "artist" and "title". Here, we use "song"
     * as the "title" field if "title" is not provided. *)
    if not (Hashtbl.mem m "title") then
      (try Hashtbl.add m "title" (Hashtbl.find m "song") with _ -> ());
    self#log#f 3 "New metadata chunk %s -- %s."
      (try Hashtbl.find m "artist" with _ -> "?")
      (try Hashtbl.find m "title" with _ -> "?") ;
    Generator.add_metadata generator m

  method get_mime_type = mime_type

  method feed socket (should_stop,has_stopped) =
    self#log#f 3 "Decoding..." ;
    let t0 = Unix.gettimeofday () in
    let read len =
      let buf = String.make len ' ' in
      let () =
        let rec wait n =
          if should_stop () then
            raise Disconnected ;
          let l,_,_ = Unix.select [socket] [] [] 1. in
            if l=[] then begin
              self#log#f 4 "No network activity for %d second(s)." n ;
              if float n >= Harbor.conf_timeout#get then
               begin
                self#log#f 4 "Network activity timeout! Disconnecting source." ;
                raise Disconnected ;
               end
              else
               wait (n+1)
            end
        in wait 1
      in
      let input = Unix.read socket buf 0 len in
      if input<=0 then raise End_of_file ;
      begin match dump with
        | Some b -> output_string b (String.sub buf 0 input)
        | None -> ()
      end ;
      begin match logf with
        | Some b ->
            let time = (Unix.gettimeofday () -. t0) /. 60. in
              Printf.fprintf b "%f %d\n%!" time self#length
        | None -> ()
      end ;
      buf,input
    in
      try
        let Decoder.Decoder decoder = create_decoder read in
        while true do
          if should_stop () then
            raise Disconnected ;
          Tutils.mutexify relay_m 
            (fun () ->
               if relay_socket = None then 
                 failwith "relaying stopped") () ;
          decoder generator
        done
      with
        | e ->
            (* Feeding has stopped: adding a break here. *)
            Generator.add_break ~sync:`Drop generator ;
            (* Do not show internal exception, e.g. Unix.read()
             * exception if source has been stopped. The socket
             * is closed when stopping so we expect this exception
             * to happen.. *)
            let e =
              match e with
                | Stopped
                | Disconnected -> e
                | _ when stopped -> Stopped
                | _ -> e
            in 
            self#log#f 2 "Feeding stopped: %s." (Utils.error_message e) ;
            (* exception Disconnected is raised when
             * the thread is being killed, which only
             * happends in self#disconnect. No need to
             * call it then.. *)
            if e <> Disconnected && e <> Stopped then
              self#disconnect ;
            has_stopped () ;
            if debug then raise e 

  method private wake_up _ =
     begin
      try
        Harbor.add_source ~port ~mountpoint ~icy (self:>Harbor.source) ;
      with
        | Harbor.Registered ->
           raise (Lang.Invalid_value
                    (List.assoc "" p,
                     (* TODO: raise two script values ? *)
                     let port = Lang.to_int (List.assoc "port" p) in
                     Printf.sprintf
                     "A source is already register for this \
                      mountpointpoint '%s' and port %i." mountpoint port))
    end ;
    (* Now we can create the log function *)
    log_ref := self#log#f 3 "%s"

  method private sleep =
    Tutils.mutexify relay_m
     (fun () ->
       (* Setting stopped to true
        * avoids deadlock with self#disconnect *)
       stopped <- true ;
       if relay_socket <> None then 
         self#disconnect) () ;
    Harbor.remove_source ~port ~mountpoint () 

  method register_decoder mime =
    Generator.set_mode generator `Undefined ;
    match
      Decoder.get_stream_decoder mime kind
    with
      | Some d -> create_decoder <- d ; mime_type <- Some mime
      | None -> raise Harbor.Unknown_codec

  method relay stype (headers:(string*string) list) socket =
    Tutils.mutexify relay_m
      (fun () ->
        if stopped then
          raise Stopped ;
        if relay_socket <> None then
          raise Harbor.Mount_taken ;
        self#register_decoder stype ;
        relay_socket <- Some socket ;
        on_connect headers ;
        begin match dumpfile with
          | Some f ->
              begin try
                dump <- Some (open_out_bin 
                                (Utils.home_unrelate f))
              with e ->
                self#log#f 2 "Could not open dump file: \
                                %s" (Utils.error_message e)
              end
          | None -> ()
        end ;
        begin match logfile with
          | Some f ->
              begin try
                logf <- Some (open_out_bin 
                                (Utils.home_unrelate f))
              with e ->
                self#log#f 2 "Could not open log file: \
                                %s" (Utils.error_message e)
              end
          | None -> ()
        end ;
        (* Wait for the old feeding thread to return, 
         * then create a new one. *)
        assert (kill_polling = None) ;
        begin match wait_polling with
          | None -> ()
          | Some f -> 
              f () ; wait_polling <- None
        end ;
        begin
         let kill,wait = 
           Tutils.stoppable_thread 
                (self#feed socket) 
                "harbor source feeding" 
         in
         kill_polling <- Some kill ;
         wait_polling <- Some wait
        end) ()

  method disconnect =
    let f () = 
      match relay_socket with
        | Some s -> 
           on_disconnect () ;
           begin match dump with
             | Some f -> 
                 close_out f ; dump <- None
             | None -> ()
           end ;
           begin match logf with
             | Some f -> 
                 close_out f ; logf <- None
             | None -> ()
           end ;
           begin 
             try
               Unix.shutdown s Unix.SHUTDOWN_ALL ;
               Unix.close s
             with _ -> ()
           end;
           (Utils.get_some kill_polling) () ;
           kill_polling <- None ;
           relay_socket <- None
        | None -> assert false
    in
    if not stopped then
      Tutils.mutexify relay_m f ()
    else
      f ()
end

let () =
  let kind = Lang.kind_type_of_kind_format ~fresh:1 Lang.audio_any in
    Lang.add_operator "input.harbor"
      ~kind:(Lang.Unconstrained kind)
      ~category:Lang.Input
      ~descr:("Retrieves the given http stream from the harbor.")
      [
        "buffer", Lang.float_t, Some (Lang.float 2.),
         Some "Duration of the pre-buffered data." ;

        "max", Lang.float_t, Some (Lang.float 10.),
        Some "Maximum duration of the buffered data.";

        "on_connect",
        Lang.fun_t [false,"",Lang.metadata_t] Lang.unit_t,
        Some (Lang.val_cst_fun ["",Lang.metadata_t,None] Lang.unit),
        Some "Function to execute when a source is connected. \
              Its receives the list of headers, of the form: \
              (<label>,<value>). All labels are lowercase.";

        "on_disconnect",Lang.fun_t [] Lang.unit_t,
        Some (Lang.val_cst_fun [] Lang.unit),
        Some "Functions to excecute when a source is disconnected";

        "user",Lang.string_t,
        Some (Lang.string "source"),
        Some "Source user.";

        "password",Lang.string_t,
        Some (Lang.string "hackme"),
        Some "Source password.";

        "port", Lang.int_t,
        Some (Lang.int 8005),
        Some "Port used to connect to the source.";

        "icy", Lang.bool_t,
        Some (Lang.bool false),
        Some "Enable ICY (shoutcast) protocol.";

        "icy_metadata_charset", Lang.string_t,
        Some (Lang.string ""),
        Some "ICY (shoutcast) metadata charset. \
              Guessed if empty. Default for shoutcast is ISO-8859-1. \
              Set to that value if all your clients send metadata using this \
              charset and automatic detection is not working for you.";

        "metadata_charset", Lang.string_t,
        Some (Lang.string ""),
        Some "Metadata charset for non-ICY (shoutcast) source protocols. \
              Guessed if empty.";

        "auth",
        Lang.fun_t [false,"",Lang.string_t;false,"",Lang.string_t] Lang.bool_t,
        Some
          (Lang.val_cst_fun
             ["",Lang.string_t,None;"",Lang.string_t,None]
             (Lang.bool false)),
        Some "Authentication function. \
              <code>f(login,password)</code> returns <code>true</code> \
              if the user should be granted access for this login. \
              Override any other method if used.";

        "dumpfile", Lang.string_t, Some (Lang.string ""),
        Some "Dump stream to file, for debugging purpose. Disabled if empty.";

        "logfile", Lang.string_t, Some (Lang.string ""),
        Some "Log buffer status to file, for debugging purpose. \
              Disabled if empty.";

        "debug", Lang.bool_t, Some (Lang.bool false),
        Some "Run in debugging mode by not catching some exceptions.";

        "", Lang.string_t, None,
        Some "Mountpoint to look for." ]
      (fun p kind ->
         let mountpoint = Lang.to_string (List.assoc "" p) in
         let mountpoint =
           if mountpoint<>"" && mountpoint.[0]='/' then mountpoint else
             Printf.sprintf "/%s" mountpoint
         in
         let trivially_false = function
           | { Lang.value =
                 Lang.Fun (_,_,_,
                           { Lang_values.term = Lang_values.Bool false }) }
               -> true
           | _ -> false
         in
         let default_user = 
           Lang.to_string (List.assoc "user" p) 
         in
         let default_password = 
           Lang.to_string (List.assoc "password" p) 
         in
         let debug = Lang.to_bool (List.assoc "debug" p) in
         let icy = Lang.to_bool (List.assoc "icy" p) in
         let icy_charset = 
           match Lang.to_string (List.assoc "icy_metadata_charset" p) with
             | "" -> None
             | s -> Some s
         in
         let meta_charset =
           match Lang.to_string (List.assoc "metadata_charset" p) with
             | "" -> None
             | s -> Some s
         in
         let port = Lang.to_int (List.assoc "port" p) in
         let auth_function = List.assoc "auth" p in
         let login user password =
           (** We try to decode user & password here. 
             * Idealy, it would be better to decode them
             * in tools/harbor.ml in order to use any
             * possible charset information there.
             * However: 
             * - ICY password are given raw, without
             *   any charset information
             * - HTTP password are encoded in Base64 and 
             *   passed through the HTTP headers, where
             *   there are no charset information concerning
             *   the password. Note: Content-Type may contain
             *   a charset information, but this refers to 
             *   the charset of the HTML content.. *)
           let user,password = 
              let f = Configure.recode_tag in
              f user, f password
           in
           let default_login =
             user = default_user &&
             password = default_password
           in
             if not (trivially_false auth_function) then
               Lang.to_bool
                 (Lang.apply ~t:Lang.bool_t
                    auth_function
                    ["",Lang.string user;
                     "",Lang.string password])
             else
               default_login
         in
         let login = (default_user, login) in
         let dumpfile =
           match Lang.to_string (List.assoc "dumpfile" p) with
             | "" -> None
             | s -> Some s
         in
         let logfile =
           match Lang.to_string (List.assoc "logfile" p) with
             | "" -> None
             | s -> Some s
         in
         let bufferize = Lang.to_float (List.assoc "buffer" p) in
         let max = Lang.to_float (List.assoc "max" p) in
         if bufferize >= max then
           raise (Lang.Invalid_value
                    (List.assoc "max" p,
                     "Maximun buffering inferior to pre-buffered data"));
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
         (new http_input_server ~kind
                   ~bufferize ~max ~login ~mountpoint
                   ~dumpfile ~logfile ~icy ~port 
                   ~icy_charset ~meta_charset
                   ~on_connect ~on_disconnect ~debug 
                   p :> Source.source))
