(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

let ( let* ) = Duppy.Monad.bind

module Http = Liq_http

(** Output to an harbor server. *)
module type T = sig
  include Harbor.Transport_t

  val source_name : string
  val source_description : string
end

module Mutex_control = struct
  type priority = Tutils.priority

  let scheduler = Tutils.scheduler
  let priority = `Non_blocking
end

module Duppy_m = Duppy.Monad.Mutex.Factory (Mutex_control)
module Duppy_c = Duppy.Monad.Condition.Factory (Duppy_m)
module Duppy = Harbor.Http_transport.Duppy

module Icecast = struct
  type protocol = unit

  let protocol_of_icecast_protocol _ = ()

  type content = string

  let format_of_content x = x

  type info = unit

  let info_of_encoder _ _ = ()
end

module M = Icecast_utils.Icecast_v (Icecast)
open M

(* Max total length for ICY metadata is 255*16
 * Format is: "StreamTitle='%s';StreamUrl='%s'"
 * "StreamTitle='';"; is 15 chars long, "StreamUrl='';"
 * is 13 chars long, leaving 4052 chars remaining.
 * Splitting those in:
 * * max title length = 3852
 * * max url length = 200 *)
let max_title = 3852
let max_url = 200

let proto frame_t =
  Output.proto
  @ Icecast_utils.base_proto frame_t
  @ [
      ("mount", Lang.string_t, None, None);
      ("port", Lang.int_t, Some (Lang.int 8000), None);
      ( "transport",
        Lang.http_transport_base_t,
        Some (Lang.base_http_transport Http.unix_transport),
        Some
          "Http transport. Use `http.transport.ssl` or \
           `http.transport.secure_transport`, when available, to enable HTTPS \
           output" );
      ( "user",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some "User for client connection. You also need to setup a `password`."
      );
      ( "password",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some
          "Password for client connection. A `user` must also be set. We check \
           for this password is checked unless an `auth` function is defined, \
           which is used in this case." );
      ( "timeout",
        Lang.float_t,
        Some (Lang.float 30.),
        Some "Timeout for network operations (in seconds)." );
      ( "encoding",
        Lang.string_t,
        Some (Lang.string ""),
        Some "Encoding used to send metadata. If empty, defaults to \"UTF-8\""
      );
      ("url", Lang.nullable_t Lang.string_t, Some Lang.null, None);
      ( "metaint",
        Lang.int_t,
        Some (Lang.int 8192),
        Some "Interval used to send ICY metadata" );
      ( "auth",
        Lang.nullable_t
          (Lang.fun_t
             [
               (false, "address", Lang.string_t);
               (false, "", Lang.string_t);
               (false, "", Lang.string_t);
             ]
             Lang.bool_t),
        Some Lang.null,
        Some
          "Authentication function. `f(~address,login,password)` returns \
           `true` if the user should be granted access for this login. When \
           defined, `user` and `password` arguments are not taken in account."
      );
      ( "buffer",
        Lang.int_t,
        Some (Lang.int (5 * 65535)),
        Some "Maximum buffer per-client." );
      ( "burst",
        Lang.int_t,
        Some (Lang.int 65534),
        Some "Initial burst of data sent to the client." );
      ( "chunk",
        Lang.int_t,
        Some (Lang.int Utils.pagesize),
        Some "Send data to clients using chunks of at least this length." );
      ( "on_connect",
        Lang.fun_t
          [
            (false, "headers", Lang.metadata_t);
            (false, "uri", Lang.string_t);
            (false, "protocol", Lang.string_t);
            (false, "", Lang.string_t);
          ]
          Lang.unit_t,
        Some
          (Lang.val_cst_fun
             [("headers", None); ("uri", None); ("protocol", None); ("", None)]
             Lang.unit),
        Some
          "Callback executed when connection is established (takes headers, \
           connection uri, protocol and client's IP as arguments)." );
      ( "on_disconnect",
        Lang.fun_t [(false, "", Lang.string_t)] Lang.unit_t,
        Some (Lang.val_cst_fun [("", None)] Lang.unit),
        Some
          "Callback executed when connection stops (takes client's IP as \
           argument)." );
      ( "headers",
        Lang.metadata_t,
        Some (Lang.list []),
        Some "Additional headers." );
      ( "dumpfile",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some "Dump stream to file, for debugging purpose. Disabled if null." );
      ("", Lang.source_t frame_t, None, None);
    ]

type client_state = Hello | Sending | Done

type metadata = {
  mutable metadata : Frame.metadata option;
  metadata_m : Mutex.t;
}

type client = {
  buffer : Strings.Mutable.t;
  condition : Duppy_c.condition;
  condition_m : Duppy_m.mutex;
  mutex : Mutex.t;
  meta : metadata;
  mutable latest_meta : string;
  metaint : int;
  timeout : float;
  url : string option;
  mutable metapos : int;
  chunk : int;
  mutable state : client_state;
  close : unit -> unit;
  handler : (Tutils.priority, Harbor.reply) Duppy.Monad.Io.handler;
}

let add_meta c data =
  let mk_icy_meta meta =
    let meta_info =
      match
        ( Frame.Metadata.find_opt "artist" meta,
          Frame.Metadata.find_opt "title" meta )
      with
        | Some a, Some t -> Some (Printf.sprintf "%s - %s" a t)
        | Some s, None | None, Some s -> Some s
        | None, None -> None
    in
    let meta =
      match meta_info with
        | Some s when String.length s > max_title ->
            Printf.sprintf "StreamTitle='%s...';"
              (String.sub s 0 (max_title - 3))
        | Some s -> Printf.sprintf "StreamTitle='%s';" s
        | None -> ""
    in
    let meta =
      match c.url with
        | Some s when String.length s > max_url ->
            Printf.sprintf "%sStreamURL='%s...';" meta
              (String.sub s 0 (max_url - 3))
        | Some s -> Printf.sprintf "%sStreamURL='%s';" meta s
        | None -> meta
    in
    (* Pad string to a multiple of 16 bytes. *)
    let len = String.length meta in
    let pad = (len / 16) + 1 in
    let ret = Bytes.make ((pad * 16) + 1) '\000' in
    Bytes.set ret 0 (Char.chr pad);
    String.blit meta 0 ret 1 len;
    let ret = Bytes.unsafe_to_string ret in
    if ret <> c.latest_meta then (
      c.latest_meta <- ret;
      ret)
    else "\000"
  in
  let get_meta () =
    let meta =
      Mutex_utils.mutexify c.meta.metadata_m
        (fun () ->
          let meta = c.meta.metadata in
          c.meta.metadata <- None;
          meta)
        ()
    in
    match meta with Some meta -> mk_icy_meta meta | None -> "\000"
  in
  let rec process cur data =
    let len = Strings.length data in
    if c.metaint <= c.metapos + len then (
      let meta = get_meta () in
      let next_meta_pos = c.metaint - c.metapos in
      let before = Strings.sub data 0 next_meta_pos in
      let after = Strings.sub data next_meta_pos (len - next_meta_pos) in
      let cur = Strings.concat [cur; before; Strings.of_string meta] in
      c.metapos <- 0;
      process cur after)
    else (
      c.metapos <- c.metapos + len;
      Strings.concat [cur; data])
  in
  if c.metaint > 0 then process Strings.empty data else data

let rec client_task c =
  let* data =
    Duppy.Monad.Io.exec ~priority:`Maybe_blocking c.handler
      (Mutex_utils.mutexify c.mutex
         (fun () ->
           let buflen = Strings.Mutable.length c.buffer in
           let data =
             if buflen > c.chunk then
               add_meta c (Strings.Mutable.flush c.buffer)
             else Strings.empty
           in
           Duppy.Monad.return data)
         ())
  in
  let* () =
    if Strings.is_empty data then
      let* () = Duppy_m.lock c.condition_m in
      let* () = Duppy_c.wait c.condition c.condition_m in
      Duppy_m.unlock c.condition_m
    else
      Duppy.Monad.Io.write ?timeout:(Some c.timeout) ~priority:`Non_blocking
        c.handler (Strings.to_bytes data)
  in
  let* state =
    Duppy.Monad.Io.exec ~priority:`Maybe_blocking c.handler
      (let ret = Mutex_utils.mutexify c.mutex (fun () -> c.state) () in
       Duppy.Monad.return ret)
  in
  if state <> Done then client_task c else Duppy.Monad.return ()

let client_task c =
  Mutex_utils.mutexify c.mutex
    (fun () ->
      assert (c.state = Hello);
      c.state <- Sending)
    ();
  Duppy.Monad.catch (client_task c) (fun _ -> Duppy.Monad.raise ())

(** Sending encoded data to a shout-compatible server.
    * It directly takes the Lang param list and extracts stuff from it. *)
class output p =
  let pos = Lang.pos p in
  let e f v = f (List.assoc v p) in
  let s v = e Lang.to_string v in
  let on_connect = List.assoc "on_connect" p in
  let on_disconnect = List.assoc "on_disconnect" p in
  let on_connect ~headers ~protocol ~uri s =
    ignore
      (Lang.apply on_connect
         [
           ("headers", Lang.metadata headers);
           ("uri", Lang.string uri);
           ("protocol", Lang.string protocol);
           ("", Lang.string s);
         ])
  in
  let on_disconnect s =
    ignore (Lang.apply on_disconnect [("", Lang.string s)])
  in
  let metaint = Lang.to_int (List.assoc "metaint" p) in
  let data = encoder_data p in
  let encoding = Lang.to_string (List.assoc "encoding" p) in
  let recode m =
    let out_enc =
      match encoding with "" -> Charset.utf8 | s -> Charset.of_string s
    in
    let f = Charset.convert ~target:out_enc in
    Frame.Metadata.fold
      (fun a b m -> Frame.Metadata.add a (f b) m)
      Frame.Metadata.empty m
  in
  let timeout = Lang.to_float (List.assoc "timeout" p) in
  let buflen = Lang.to_int (List.assoc "buffer" p) in
  let burst = Lang.to_int (List.assoc "burst" p) in
  let chunk = Lang.to_int (List.assoc "chunk" p) in
  let () =
    if chunk > buflen then
      raise
        (Error.Invalid_value
           (List.assoc "buffer" p, "Maximum buffering inferior to chunk length"))
    else ();
    if burst > buflen then
      raise
        (Error.Invalid_value
           (List.assoc "buffer" p, "Maximum buffering inferior to burst length"))
    else ()
  in
  let source = Lang.assoc "" 2 p in
  let mount = s "mount" in
  let uri =
    match mount.[0] with '/' -> mount | _ -> Printf.sprintf "%c%s" '/' mount
  in
  let uri =
    let regexp = [%string {|^%{uri}$|}] in
    Liquidsoap_lang.Builtins_regexp.
      { descr = regexp; flags = []; regexp = Re.Pcre.regexp regexp }
  in
  let autostart = Lang.to_bool (List.assoc "start" p) in
  let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
  let register_telnet = Lang.to_bool (List.assoc "register_telnet" p) in
  let on_start =
    let f = List.assoc "on_start" p in
    fun () -> ignore (Lang.apply f [])
  in
  let on_stop =
    let f = List.assoc "on_stop" p in
    fun () -> ignore (Lang.apply f [])
  in
  let url = List.assoc "url" p |> Lang.to_option |> Option.map Lang.to_string in
  let port = e Lang.to_int "port" in
  let transport = e Lang.to_http_transport "transport" in
  let default_user =
    List.assoc "user" p |> Lang.to_option |> Option.map Lang.to_string
  in
  let default_password =
    List.assoc "password" p |> Lang.to_option |> Option.map Lang.to_string
  in
  let address_resolver s =
    let s = Harbor.file_descr_of_socket s in
    Utils.name_of_sockaddr ~rev_dns:Harbor_base.conf_revdns#get
      (Unix.getpeername s)
  in
  let auth_function = List.assoc "auth" p |> Lang.to_option in
  let login ~socket user password =
    let address = address_resolver socket in
    let user, password =
      let f = Charset.convert in
      (f user, f password)
    in
    match auth_function with
      | Some f ->
          Lang.to_bool
            (Lang.apply f
               [
                 ("address", Lang.string address);
                 ("", Lang.string user);
                 ("", Lang.string password);
               ])
      | None -> (
          match (default_user, default_password) with
            | _, None -> false
            | None, _ -> false
            | Some default_user, Some default_password ->
                user = default_user && password = default_password)
  in
  let dumpfile =
    Lang.to_valued_option Lang.to_string (List.assoc "dumpfile" p)
  in
  let extra_headers =
    List.map
      (fun v ->
        let f (x, y) = (Lang.to_string x, Lang.to_string y) in
        f (Lang.to_product v))
      (Lang.to_list (List.assoc "headers" p))
  in
  object (self)
    (** File descriptor where to dump. *)
    inherit
      [Strings.t] Output.encoded
        ~output_kind:"output.harbor" ~infallible ~register_telnet ~autostart
          ~export_cover_metadata:false ~on_start ~on_stop ~name:mount source

    val mutable dump = None
    val mutable encoder = None
    val mutable clients = Queue.create ()
    val clients_m = Mutex.create ()
    val duppy_c = Duppy_c.create ()
    val duppy_m = Duppy_m.create ()
    val mutable chunk_len = 0
    val burst_data = Strings.Mutable.empty ()
    val metadata = { metadata = None; metadata_m = Mutex.create () }
    method encode frame = (Option.get encoder).Encoder.encode frame

    method insert_metadata m =
      let m = Frame.Metadata.Export.to_metadata m in
      let m = recode m in
      Mutex_utils.mutexify metadata.metadata_m
        (fun () -> metadata.metadata <- Some m)
        ();
      (Option.get encoder).Encoder.insert_metadata
        (Frame.Metadata.Export.from_metadata ~cover:false m)

    method add_client ~protocol ~headers ~uri ~query s =
      let ip =
        (* Show port = true to catch different clients from same ip *)
        let fd = Harbor.file_descr_of_socket s in
        Utils.name_of_sockaddr ~show_port:true (Unix.getpeername fd)
      in
      let metaint, icyheader =
        try
          assert (List.assoc "Icy-MetaData" headers = "1");
          (metaint, Printf.sprintf "icy-metaint: %d\r\n" metaint)
        with _ -> (-1, "")
      in
      let extra_headers =
        String.concat ""
          (List.map
             (fun (x, y) -> Printf.sprintf "%s: %s\r\n" x y)
             extra_headers)
      in
      let reply =
        Printf.sprintf "HTTP/%s 200 OK\r\nContent-type: %s\r\n%s%s\r\n" protocol
          data.format icyheader extra_headers
      in
      let buffer =
        Strings.Mutable.of_strings ((Option.get encoder).Encoder.header ())
      in
      let close () = try Harbor.close s with _ -> () in
      let rec client =
        {
          buffer;
          condition = duppy_c;
          condition_m = duppy_m;
          metaint;
          meta = metadata;
          latest_meta = "\000";
          metapos = 0;
          url;
          timeout;
          mutex = Mutex.create ();
          state = Hello;
          chunk;
          close;
          handler;
        }
      and handler =
        {
          Duppy.Monad.Io.scheduler = Tutils.scheduler;
          socket = s;
          data = "";
          on_error =
            (fun e ->
              let bt = Printexc.get_backtrace () in
              let msg =
                match e with
                  | Duppy.Io.Timeout -> Printf.sprintf "Timeout error for %s" ip
                  | Duppy.Io.Io_error -> Printf.sprintf "I/O error for %s" ip
                  | Duppy.Io.Unix (c, p, m) ->
                      Printf.sprintf "Unix error for %s: %s" ip
                        (Printexc.to_string (Unix.Unix_error (c, p, m)))
                  | Duppy.Io.Unknown e ->
                      Printf.sprintf "%s" (Printexc.to_string e)
              in
              Utils.log_exception ~log:self#log ~bt msg;
              self#log#info "Client %s disconnected" ip;
              Mutex_utils.mutexify client.mutex
                (fun () ->
                  client.state <- Done;
                  ignore (Strings.Mutable.flush client.buffer))
                ();
              on_disconnect ip;
              Harbor.Close (Harbor.mk_simple ""));
        }
      in
      self#log#info "Serving client %s." ip;
      let* () =
        Duppy.Monad.catch
          (if
             (default_user <> None && default_password <> None)
             || auth_function <> None
           then (
             let default_user = Option.value default_user ~default:"" in
             Duppy.Monad.Io.exec ~priority:`Maybe_blocking handler
               (Harbor.http_auth_check ~query ~login:(default_user, login) s
                  headers))
           else Duppy.Monad.return ())
          (function
            | Harbor.Close s ->
                self#log#info "Client %s failed to authenticate!" ip;
                client.state <- Done;
                Harbor.reply s
            | _ -> assert false)
      in
      Duppy.Monad.Io.exec ~priority:`Maybe_blocking handler
        (Harbor.relayed reply (fun () ->
             self#log#info "Client %s connected" ip;
             Mutex_utils.mutexify clients_m
               (fun () -> Queue.push client clients)
               ();
             on_connect ~protocol ~uri
               ~headers:(Frame.Metadata.from_list headers)
               ip))

    method send b =
      let slen = Strings.length b in
      if slen > 0 then (
        chunk_len <- chunk_len + slen;
        let wake_up =
          if chunk_len >= chunk then (
            chunk_len <- 0;
            true)
          else false
        in
        Strings.Mutable.append_strings burst_data b;
        Strings.Mutable.keep burst_data burst;
        let new_clients = Queue.create () in
        (match dump with
          | Some s -> Strings.iter (output_substring s) b
          | None -> ());
        Mutex_utils.mutexify clients_m
          (fun () ->
            Queue.iter
              (fun c ->
                let start =
                  Mutex_utils.mutexify c.mutex
                    (fun () ->
                      match c.state with
                        | Hello ->
                            Strings.Mutable.append c.buffer burst_data;
                            Queue.push c new_clients;
                            true
                        | Sending ->
                            let buf = Strings.Mutable.length c.buffer in
                            if buf + slen > buflen then
                              Strings.Mutable.drop c.buffer (min buf slen);
                            Strings.Mutable.append_strings c.buffer b;
                            Queue.push c new_clients;
                            false
                        | Done -> false)
                    ()
                in
                if start then
                  Duppy.Monad.run ~return:c.close ~raise:c.close (client_task c)
                else ())
              clients;
            if wake_up && Queue.length new_clients > 0 then
              Duppy.Monad.run
                ~return:(fun () -> ())
                ~raise:(fun () -> ())
                (Duppy_c.broadcast duppy_c)
            else ();
            clients <- new_clients)
          ())
      else ()

    method start =
      assert (encoder = None);
      let enc = data.factory self#id in
      encoder <- Some (enc Frame.Metadata.Export.empty);
      let handler ~protocol ~meth:_ ~data:_ ~headers ~query ~socket uri =
        self#add_client ~protocol ~headers ~uri ~query socket
      in
      Harbor.add_http_handler ~pos ~transport ~port ~verb:`Get ~uri handler;
      match dumpfile with Some f -> dump <- Some (open_out_bin f) | None -> ()

    method stop =
      ignore ((Option.get encoder).Encoder.stop ());
      encoder <- None;
      Harbor.remove_http_handler ~port ~verb:`Get ~uri ();
      let new_clients = Queue.create () in
      Mutex_utils.mutexify clients_m
        (fun () ->
          Queue.iter
            (fun c ->
              Mutex_utils.mutexify c.mutex
                (fun () ->
                  c.state <- Done;
                  Duppy.Monad.run
                    ~return:(fun () -> ())
                    ~raise:(fun () -> ())
                    (Duppy_c.broadcast duppy_c))
                ())
            clients;
          clients <- new_clients)
        ();
      match dump with Some f -> close_out f | None -> ()

    method! reset =
      self#stop;
      self#start
  end

let _ =
  let return_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  Lang.add_operator ~category:`Output
    ~descr:"Encode and output the stream using the harbor server."
    ~meth:Output.meth ~base:Modules.output "harbor" (proto return_t) ~return_t
    (fun p -> (new output p :> Output.output))
