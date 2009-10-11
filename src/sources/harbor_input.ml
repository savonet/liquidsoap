(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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
open Http_source


(* {1 Input handling} *)

exception NoDecoder

class http_input_server ~dumpfile ~logfile
                        ~bufferize ~max
                        ~on_connect ~on_disconnect
                        ~login ~debug =
  let abg_max_len = Fmt.samples_of_seconds max in
object (self)
  inherit Source.source
  inherit Generated.source
            (Generator.create ())
            ~empty_on_abort:false ~bufferize

  val mutable relaying = false
  val mutable ns = []
  val mutable decoder = fun _ -> raise NoDecoder
  val mutable stype = None

  val mutable dump = None
  val mutable logf = None

  method login : (string option)*(string -> string -> bool) = login

  method stype = Source.Fallible

  (* Insert metadata *)
  method insert_metadata m =
    self#log#f 3 "New metadata chunk \"%s -- %s\""
      (try Hashtbl.find m "artist" with _ -> "?")
      (try Hashtbl.find m "title" with _ -> "?") ;
    Generator.add_metadata abg m

  method put sample_freq data =
    if not relaying then failwith "relaying stopped" ;
    Mutex.lock lock ;
    (* TODO There must be two ways of handling overfull generator:
     * (1) when streaming, one should just stop the decoder for a while;
     * (2) when not streaming, one should throw some data.
     * Doing 1 instead of 2 can lead to deconnections.
     * Doing 2 instead of 1 leads to ugly sound.
     * Here, we drop data since we want to remain
     * connected to the client. *)
    if Generator.length abg >= abg_max_len then
      begin
        Mutex.unlock lock ;
        Thread.delay (max /. 3.) ;
        Mutex.lock lock ;
        if Generator.length abg >= abg_max_len then
        (* Here, we drop some data after the maximun buffer has been filled.
         * Delaying the function can lead to deconnection/connection cycles
         * when the source is not pulled. *)
          Generator.remove abg (Generator.length abg - abg_max_len)
      end ;
    Generator.feed abg ~sample_freq data ;
    Mutex.unlock lock

  method register_decoder s =
    match
      Http_source.stream_decoders#get s
    with
      | Some d -> decoder <- d ; stype <- Some s
      | None -> raise Harbor.Unknown_codec

  method get_type = stype

  method feed socket  =
    self#log#f 3 "Decoding..." ;
    let close () = () in
    let t0 = Unix.gettimeofday () in
    let read len =
      let buf = String.make len ' ' in
      let () =
        let rec wait n =
          let l,_,_ = Unix.select [socket] [] [] 1. in
            if l=[] then begin
              self#log#f 4 "No network activity for %d second(s)." n ;
              if float n >= Harbor.conf_timeout#get then
               begin
                self#log#f 4 "network activity timeout! disconnecting source" ;
                self#disconnect
               end
              else
               wait (n+1)
            end
        in wait 1
      in
      let input = Unix.read socket buf 0 len in
      if input<=0 then raise End_of_file ;
      let s = String.sub buf 0 input in
      begin match dump with
        | Some b -> output_string b s
        | None -> ()
      end ;
      begin match logf with
        | Some b ->
            let time = (Unix.gettimeofday () -. t0) /. 60. in
              Printf.fprintf b "%f %d\n%!" time self#length
        | None -> ()
      end ;
      s
    in
    let sink =
      { put = self#put ; read = read ;
        insert_metadata = self#insert_metadata ; close = close }
    in
     begin
      try decoder sink with
        | e -> self#log#f 2 "Feeding stopped: %s" (Printexc.to_string e) ;
               if debug then raise e
     end;
    self#disconnect ;
    try
      Unix.close socket
    with
      | _ -> ()

  method private wake_up _ =
    if ns = [] then
      ns <- Server.register [self#id] "input.harbor" ;
    self#set_id (Server.to_string ns) ;
    let stop _ =
      if relaying then (self#disconnect ; "Done")
      else "No source client connected"
    in
    Server.add
      ~ns "stop" ~descr:"Stop current source client, if connected." stop ;
    Server.add
      ~ns "kick" ~descr:"Kick current source client, if connected." stop ;
    Server.add
      ~ns "status" ~descr:"Display current status."
      (fun _ ->
         if relaying then
           "source client connected"
         else
           "no source client connected") ;
    Server.add ~ns "buffer_length" ~usage:"buffer_length"
               ~descr:"Get the buffer's length, in seconds."
       (fun _ -> Printf.sprintf "%.2f"
             (Fmt.seconds_of_samples self#length))

  method private sleep =
    if relaying then self#disconnect

  method relay (headers:(string*string) list) socket =
    relaying <- true ;
    let headers = List.map (fun (x,y) -> String.lowercase x,y) headers in
    on_connect headers ;
    begin match dumpfile with
      | Some f ->
          begin try
            dump <- Some (open_out_bin (Utils.home_unrelate f))
          with e ->
            self#log#f 2 "Could not open dump file: %s" (Printexc.to_string e)
          end
      | None -> ()
    end ;
    begin match logfile with
      | Some f ->
          begin try
            logf <- Some (open_out_bin (Utils.home_unrelate f))
          with e ->
            self#log#f 2 "Could not open log file: %s" (Printexc.to_string e)
          end
      | None -> ()
    end ;
    ignore (Tutils.create
              (fun () -> self#feed socket) ()
              "harbor source feeding")

  method disconnect =
    if relaying then on_disconnect () ;
    begin match dump with
      | Some f -> close_out f ; dump <- None
      | None -> ()
    end ;
    begin match logf with
      | Some f -> close_out f ; logf <- None
      | None -> ()
    end ;
    relaying <- false

  method is_taken = relaying

end

let () =
    Lang.add_operator "input.harbor"
      ~category:Lang.Input
      ~descr:("Retrieves the given http stream from the harbor.")
      [
        "buffer", Lang.float_t, Some (Lang.float 2.),
         Some "Duration of the pre-buffered data." ;

        "max", Lang.float_t, Some (Lang.float 10.),
        Some "Maximum duration of the buffered data.";

        "on_connect", Lang.fun_t 
        [false,"",Lang.list_t (Lang.product_t Lang.string_t Lang.string_t)] Lang.unit_t,
        Some (Lang.val_cst_fun 
          ["","headers",None] Lang.unit),
        Some "Function to execute when a source is connected. \
              Its receives the list of headers, of the form: \
              (\"label\",\"value\"). All labels are lowercase.";

        "on_disconnect",Lang.fun_t [] Lang.unit_t,
        Some (Lang.val_cst_fun [] Lang.unit),
        Some "Functions to excecute when a source is disconnected";

        "user",Lang.string_t,
        Some (Lang.string ""),
        Some "Source user. Override default if not empty.";

        "password",Lang.string_t,
        Some (Lang.string ""),
        Some "Source password. Override default if not empty.";

        "auth",Lang.fun_t [false,"",Lang.string_t;false,"",Lang.string_t]
                                    Lang.bool_t,
        Some (Lang.val_cst_fun
                ["","login",None;"","password",None]
                (Lang.bool false)),
        Some "Authentification function. \
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
      (fun p _ ->
         let mount = Lang.to_string (List.assoc "" p) in
         let mount =
           if mount<>"" && mount.[0]='/' then mount else
             Printf.sprintf "/%s" mount
         in
         let trivially_false = function
           | { Lang.value =
                 Lang.Fun (_,_,_,
                           { Lang_values.term = Lang_values.Bool false }) }
               -> true
           | _ -> false
         in
         let user = Lang.to_string (List.assoc "user" p) in
         let password = Lang.to_string (List.assoc "password" p) in
         let debug = Lang.to_bool (List.assoc "debug" p) in
         let auth_function = List.assoc "auth" p in
         let login user pass =
           let user_login test_user test_pass =
             let user,pass =
               let f g x = match x with "" -> g | _ -> x in
               f Harbor.conf_harbor_user#get user,
               f Harbor.conf_harbor_pass#get password
             in
             test_user = user &&
             test_pass = pass
           in
             if not (trivially_false auth_function) then
               Lang.to_bool
                 (Lang.apply auth_function
                    ["",Lang.string user;
                     "",Lang.string pass])
             else
               user_login user pass
         in
         let login =
           let f x = if x <> "" then Some x else None in
           (f user, login)
         in
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
         if bufferize > max then
           raise (Lang.Invalid_value
                    (List.assoc "max" p,
                     "Maximun buffering inferior to pre-buffered data"));
         let on_connect l =
           let l = 
             List.map 
              (fun (x,y) -> Lang.product (Lang.string x) (Lang.string y))
              l
           in
           let arg = Lang.list l in
           ignore (Lang.apply (List.assoc "on_connect" p) ["",arg])
         in
         let on_disconnect = fun () -> ignore (Lang.apply
                               (List.assoc "on_disconnect" p) []) in
           try
             ((Harbor.find_source mount):>Source.source)
           with
             | Not_found ->
                 Harbor.add_source mount
                   ((new http_input_server
                       ~bufferize ~max ~login
                       ~dumpfile ~logfile
                       ~on_connect ~on_disconnect ~debug):>Harbor.source) ;
                 ((Harbor.find_source mount):>Source.source))
