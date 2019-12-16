(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

module Generator = Generator.From_audio_video_plus
module Generated = Generated.Make (Generator)

module type T = sig
  include Harbor.T

  val source_name : string
  val source_description : string
end

module Make (Harbor : T) = struct
  class http_input_server ~kind ~dumpfile ~logfile ~bufferize ~max ~icy ~port
    ~meta_charset ~icy_charset ~replay_meta ~mountpoint ~on_connect
    ~on_disconnect ~login ~debug ~timeout p =
    let max_ticks = Frame.master_of_seconds max in
    (* We need a temporary log until
     * the source has an id *)
    let log_ref = ref (fun _ -> ()) in
    let log x = !log_ref x in
    let abg =
      Generator.create ~log ~kind ~overfull:(`Drop_old max_ticks) `Undefined
    in
    object (self)
      inherit Source.source ~name:Harbor.source_name kind as super

      inherit Generated.source abg ~empty_on_abort:false ~replay_meta ~bufferize

      val mutable relay_socket = None

      (** Function to read on socket. *)
      val mutable relay_read = fun _ _ _ -> assert false

      (* Mutex used to protect socket's state (close) *)
      val relay_m = Mutex.create ()

      val mutable create_decoder = fun _ -> assert false

      val mutable mime_type = None

      val mutable dump = None

      val mutable logf = None

      initializer
      ns_kind <- "input.harbor";
      let stop _ =
        self#disconnect ~lock:true;
        "Done"
      in
      self#register_command "stop"
        ~descr:"Stop current source client, if connected." stop;
      self#register_command "status" ~descr:"Display current status."
        (Tutils.mutexify relay_m (fun _ ->
             match relay_socket with
               | Some s ->
                   let s = Harbor.file_descr_of_socket s in
                   Printf.sprintf "source client connected from %s"
                     (Utils.name_of_sockaddr
                        ~rev_dns:Harbor_base.conf_revdns#get
                        (Unix.getpeername s))
               | None -> "no source client connected"));
      self#register_command "buffer_length" ~usage:"buffer_length"
        ~descr:"Get the buffer's length, in seconds." (fun _ ->
          Printf.sprintf "%.2f" (Frame.seconds_of_audio self#length))

      method login : string * (string -> string -> bool) = login

      method stype = Source.Fallible

      method icy_charset = icy_charset

      method meta_charset = meta_charset

      (* Insert metadata *)
      method insert_metadata m =
        (* Metadata may contain only the "song" value
         * or "artist" and "title". Here, we use "song"
         * as the "title" field if "title" is not provided. *)
        if not (Hashtbl.mem m "title") then (
          try Hashtbl.add m "title" (Hashtbl.find m "song") with _ -> () );
        self#log#important "New metadata chunk %s -- %s."
          (try Hashtbl.find m "artist" with _ -> "?")
          (try Hashtbl.find m "title" with _ -> "?");
        Generator.add_metadata generator m

      method get_mime_type = mime_type

      method feed =
        self#log#important "Decoding...";
        let t0 = Unix.gettimeofday () in
        let read buf ofs len =
          let input =
            (fun buf len ->
              let socket =
                Tutils.mutexify relay_m (fun () -> relay_socket) ()
              in
              match socket with
                | None -> 0
                | Some socket -> (
                    try
                      let rec f () =
                        try
                          let fd = Harbor.file_descr_of_socket socket in
                          (* Wait for `Read event on socket. *)
                          Tutils.wait_for ~log (`Read fd) timeout;

                          (* Now read. *)
                          relay_read socket buf ofs len
                        with Harbor.Retry -> f ()
                      in
                      f ()
                    with e ->
                      self#log#severe "Error while reading from client: %s"
                        (Printexc.to_string e);
                      self#disconnect ~lock:false;
                      0 ))
              buf len
          in
          begin
            match dump with
            | Some b -> output_string b (Bytes.sub_string buf 0 input)
            | None -> ()
          end;
          begin
            match logf with
            | Some b ->
                let time = (Unix.gettimeofday () -. t0) /. 60. in
                Printf.fprintf b "%f %d\n%!" time self#length
            | None -> ()
          end;
          input
        in
        let input =
          { Decoder.read; tell = None; length = None; lseek = None }
        in
        try
          let decoder = create_decoder input in
          while true do
            Tutils.mutexify relay_m
              (fun () ->
                if relay_socket = None then failwith "relaying stopped")
              ();
            decoder.Decoder.decode generator
          done
        with e ->
          (* Feeding has stopped: adding a break here. *)
          Generator.add_break ~sync:`Drop generator;
          self#log#severe "Feeding stopped: %s." (Printexc.to_string e);
          self#disconnect ~lock:true;
          if debug then raise e

      method private wake_up act =
        super#wake_up act;
        begin
          try Harbor.add_source ~port ~mountpoint ~icy (self :> Harbor.source)
          with Harbor.Registered ->
            raise
              (Lang_errors.Invalid_value
                 ( List.assoc "" p,
                   (* TODO: raise two script values ? *)
                   let port = Lang.to_int (List.assoc "port" p) in
                   Printf.sprintf
                     "A source is already register for this mountpointpoint \
                      '%s' and port %i."
                     mountpoint port ))
        end;

        (* Now we can create the log function *)
        log_ref := fun s -> self#log#important "%s" s

      method private sleep =
        self#disconnect ~lock:true;
        Harbor.remove_source ~port ~mountpoint ()

      method register_decoder mime =
        let mime =
          try
            let sub = Pcre.exec ~pat:"^([^;]+);.*$" mime in
            Pcre.get_substring sub 1
          with Not_found -> mime
        in
        Generator.set_mode generator `Undefined;
        match Decoder.get_stream_decoder mime kind with
          | Some d ->
              create_decoder <- d;
              mime_type <- Some mime
          | None -> raise Harbor.Unknown_codec

      method relay stype (headers : (string * string) list)
          ?(read = Harbor.read) socket =
        Tutils.mutexify relay_m
          (fun () ->
            if relay_socket <> None then raise Harbor.Mount_taken;
            self#register_decoder stype;
            relay_socket <- Some socket;
            relay_read <- read)
          ();
        on_connect headers;
        begin
          match dumpfile with
          | Some f -> (
              try dump <- Some (open_out_bin (Utils.home_unrelate f))
              with e ->
                self#log#severe "Could not open dump file: %s"
                  (Printexc.to_string e) )
          | None -> ()
        end;
        begin
          match logfile with
          | Some f -> (
              try logf <- Some (open_out_bin (Utils.home_unrelate f))
              with e ->
                self#log#severe "Could not open log file: %s"
                  (Printexc.to_string e) )
          | None -> ()
        end;
        ignore (Tutils.create (fun () -> self#feed) () "harbor source feeding")

      method private disconnect_no_lock =
        Utils.maydo (fun s -> try Harbor.close s with _ -> ()) relay_socket;
        relay_socket <- None

      method private disconnect_with_lock =
        Tutils.mutexify relay_m (fun () -> self#disconnect_no_lock) ()

      method private after_disconnect =
        begin
          match dump with
          | Some f ->
              close_out f;
              dump <- None
          | None -> ()
        end;
        begin
          match logf with
          | Some f ->
              close_out f;
              logf <- None
          | None -> ()
        end;
        on_disconnect ()

      method disconnect ~lock : unit =
        if lock then self#disconnect_with_lock else self#disconnect_no_lock;
        self#after_disconnect
    end

  let () =
    Lang.add_operator Harbor.source_name
      ~kind:(Lang.Unconstrained (Lang.univ_t ()))
      ~category:Lang.Input ~descr:Harbor.source_description
      [
        ( "buffer",
          Lang.float_t,
          Some (Lang.float 2.),
          Some "Duration of the pre-buffered data." );
        ( "max",
          Lang.float_t,
          Some (Lang.float 10.),
          Some "Maximum duration of the buffered data." );
        ( "timeout",
          Lang.float_t,
          Some (Lang.float 30.),
          Some "Timeout for source connectionn." );
        ( "on_connect",
          Lang.fun_t [(false, "", Lang.metadata_t)] Lang.unit_t,
          Some (Lang.val_cst_fun [("", Lang.metadata_t, None)] Lang.unit),
          Some
            "Function to execute when a source is connected. Its receives the \
             list of headers, of the form: (<label>,<value>). All labels are \
             lowercase." );
        ( "on_disconnect",
          Lang.fun_t [] Lang.unit_t,
          Some (Lang.val_cst_fun [] Lang.unit),
          Some "Functions to excecute when a source is disconnected" );
        ("user", Lang.string_t, Some (Lang.string "source"), Some "Source user.");
        ( "password",
          Lang.string_t,
          Some (Lang.string "hackme"),
          Some "Source password." );
        ( "port",
          Lang.int_t,
          Some (Lang.int 8005),
          Some "Port used to connect to the source." );
        ( "icy",
          Lang.bool_t,
          Some (Lang.bool false),
          Some "Enable ICY (shoutcast) protocol." );
        ( "icy_metadata_charset",
          Lang.string_t,
          Some (Lang.string ""),
          Some
            "ICY (shoutcast) metadata charset. Guessed if empty. Default for \
             shoutcast is ISO-8859-1. Set to that value if all your clients \
             send metadata using this charset and automatic detection is not \
             working for you." );
        ( "metadata_charset",
          Lang.string_t,
          Some (Lang.string ""),
          Some
            "Metadata charset for non-ICY (shoutcast) source protocols. \
             Guessed if empty." );
        ( "replay_metadata",
          Lang.bool_t,
          Some (Lang.bool false),
          Some
            "Replay last known metadata when switching back to this source. \
             This helps when source has dropped due to temporary connection \
             issues." );
        ( "auth",
          Lang.fun_t
            [(false, "", Lang.string_t); (false, "", Lang.string_t)]
            Lang.bool_t,
          Some
            (Lang.val_cst_fun
               [("", Lang.string_t, None); ("", Lang.string_t, None)]
               (Lang.bool false)),
          Some
            "Authentication function. `f(login,password)` returns `true` if \
             the user should be granted access for this login. Override any \
             other method if used." );
        ( "dumpfile",
          Lang.string_t,
          Some (Lang.string ""),
          Some "Dump stream to file, for debugging purpose. Disabled if empty."
        );
        ( "logfile",
          Lang.string_t,
          Some (Lang.string ""),
          Some
            "Log buffer status to file, for debugging purpose. Disabled if \
             empty." );
        ( "debug",
          Lang.bool_t,
          Some (Lang.bool false),
          Some "Run in debugging mode by not catching some exceptions." );
        ("", Lang.string_t, None, Some "Mountpoint to look for.");
      ]
      (fun p kind ->
        let mountpoint = Lang.to_string (List.assoc "" p) in
        let mountpoint =
          if mountpoint <> "" && mountpoint.[0] = '/' then mountpoint
          else Printf.sprintf "/%s" mountpoint
        in
        let trivially_false = function
          | {
              Lang.value =
                Lang.Fun
                  (_, _, _, { Lang_values.term = Lang_values.Bool false; _ });
              _;
            } ->
              true
          | _ -> false
        in
        let default_user = Lang.to_string (List.assoc "user" p) in
        let default_password = Lang.to_string (List.assoc "password" p) in
        let debug = Lang.to_bool (List.assoc "debug" p) in
        let timeout = Lang.to_float (List.assoc "timeout" p) in
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
        let replay_meta = Lang.to_bool (List.assoc "replay_metadata" p) in
        let port = Lang.to_int (List.assoc "port" p) in
        let auth_function = List.assoc "auth" p in
        let login user password =
          (* We try to decode user & password here.
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
          let user, password =
            let f = Configure.recode_tag in
            (f user, f password)
          in
          let default_login =
            user = default_user && password = default_password
          in
          if not (trivially_false auth_function) then
            Lang.to_bool
              (Lang.apply ~t:Lang.bool_t auth_function
                 [("", Lang.string user); ("", Lang.string password)])
          else default_login
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
          raise
            (Lang_errors.Invalid_value
               ( List.assoc "max" p,
                 "Maximum buffering inferior to pre-buffered data" ));
        let on_connect l =
          let l =
            List.map
              (fun (x, y) -> Lang.product (Lang.string x) (Lang.string y))
              l
          in
          let arg =
            Lang.list ~t:(Lang.product_t Lang.string_t Lang.string_t) l
          in
          ignore
            (Lang.apply ~t:Lang.unit_t (List.assoc "on_connect" p) [("", arg)])
        in
        let on_disconnect () =
          ignore (Lang.apply ~t:Lang.unit_t (List.assoc "on_disconnect" p) [])
        in
        ( new http_input_server
            ~kind ~timeout ~bufferize ~max ~login ~mountpoint ~dumpfile ~logfile
            ~icy ~port ~icy_charset ~meta_charset ~replay_meta ~on_connect
            ~on_disconnect ~debug p
          :> Source.source ))
end

module Unix_input = struct
  include Harbor

  let source_name = "input.harbor"

  let source_description =
    "Create a source that receives a http/icecast stream and forwards it as a \
     stream."
end

module Unix = Make (Unix_input)
include Unix
