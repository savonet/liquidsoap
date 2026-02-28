(**************************************************************************)
(*  ocaml-dtools                                                          *)
(*  Copyright (C) 2003-2010  The Savonet Team                             *)
(**************************************************************************)
(*  This program is free software; you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation; either version 2 of the License, or     *)
(*  any later version.                                                    *)
(**************************************************************************)
(*  Contact: savonet-devl@lists.sourceforge.net                           *)
(**************************************************************************)

(* $Id$ *)

(** ocaml-dtools
    @author Stephane Gimenez *)

module Conf = struct
  type link = string

  type path = link list

  and ut =
    < kind : string option
    ; descr : string
    ; comments : string list
    ; plug : string -> ut -> unit
    ; subs : string list
    ; path : string list -> ut
    ; routes : ut -> path list
    ; ut : ut >

  type 'a t =
    < kind : string option
    ; alias : ?comments:string list -> ?descr:string -> (ut -> unit) -> 'a t
    ; descr : string
    ; comments : string list
    ; plug : string -> ut -> unit
    ; subs : string list
    ; path : string list -> ut
    ; routes : ut -> path list
    ; ut : ut
    ; set_d : 'a option -> unit
    ; get_d : 'a option
    ; set : 'a -> unit
    ; get : 'a
    ; is_set : bool
    ; validate : ('a -> bool) -> unit
    ; on_change : ('a -> unit) -> unit >

  type links = (string * ut) list

  type 'a builder =
    ?d:'a ->
    ?p:(ut -> unit) ->
    ?l:links ->
    ?comments:string list ->
    string ->
    'a t

  exception Undefined of ut
  exception Invalid of string
  exception Unbound of ut * string
  exception Bound of ut * string
  exception Mismatch of ut
  exception Wrong_Conf of string * string
  exception File_Wrong_Conf of string * int * string
  exception Cyclic of ut * ut
  exception Invalid_Value of ut

  let path_sep_regexp = Str.regexp "\\."
  let list_sep_regexp = Str.regexp ":"

  let line_regexp =
    Str.regexp
      "^[ \t]*\\([a-zA-Z]+\\)[ \t]+\\([a-zA-Z0-9._-]+\\)[ \t]*:\\(.*\\)$"

  let comment_regexp = Str.regexp "^[ ]*\\(#.*\\)?$"
  let check s = if Str.string_match path_sep_regexp s 0 then raise (Invalid s)

  let make kind ?(d : 'a option) ?(p : ut -> unit = fun _ -> ())
      ?(l : links = []) ?(comments : string list = []) descr : 'a t =
    object (self)
      val kind : string option = kind
      val mutable descr : string = descr
      val mutable comments : string list = comments
      val mutable links : links = []
      val value_d : 'a option ref = ref d
      val value : 'a option ref = ref None
      val mutable validators : ('a -> bool) list = []
      val mutable listeners : ('a -> unit) list = []

      initializer
        p self#ut;
        List.iter (fun (s, t) -> self#plug s t) l

      method subs = List.sort compare (List.map fst links)

      method private sub (s : string) : ut =
        check s;
        try List.assoc s links with Not_found -> raise (Unbound (self#ut, s))

      method path (l : string list) : ut =
        match l with [] -> self#ut | s :: q -> (self#sub s)#path q

      method routes (st : ut) =
        (* todo: cache already accessed nodes *)
        let rec aux l t =
          match t = st with
            | true -> [List.rev l]
            | false ->
                List.concat
                  (List.map (fun s -> aux (s :: l) (t#path [s])) t#subs)
        in
        aux [] self#ut

      method kind = kind
      method descr = descr
      method private set_descr new_descr = descr <- new_descr
      method comments = comments
      method private set_comments new_comments = comments <- new_comments

      method plug s t =
        if t#routes self#ut <> [] then raise (Cyclic (self#ut, t));
        if List.mem_assoc s links then raise (Bound (self#ut, s));
        links <- (s, t) :: links

      (* Nice hack. heh! *)
      method alias ?comments ?descr p =
        let maybe f x = match x with Some x -> f x | None -> () in
        let old_comments = self#comments in
        let old_descr = self#descr in
        maybe self#set_comments comments;
        maybe self#set_descr descr;
        let key = Oo.copy self in
        p key#ut;
        self#set_comments old_comments;
        self#set_descr old_descr;
        key

      method ut = (self :> ut)
      method get_d : 'a option = !value_d
      method set_d (v : 'a option) : unit = value_d := v

      method get : 'a =
        match !value with
          | None -> (
              match !value_d with
                | None -> raise (Undefined self#ut)
                | Some v -> v)
          | Some v -> v

      method set (v : 'a) : unit =
        List.iter
          (fun fn -> if not (fn v) then raise (Invalid_Value self#ut))
          validators;
        value := Some v;
        List.iter (fun fn -> fn v) listeners

      method is_set : bool = !value <> None
      method validate (fn : 'a -> bool) : unit = validators <- fn :: validators
      method on_change (fn : 'a -> unit) : unit = listeners <- fn :: listeners
    end

  let void ?p ?l ?comments descr = (make None ?p ?l ~d:None ?comments descr)#ut
  let unit ?d = make (Some "unit") ?d
  let int ?d = make (Some "int") ?d
  let float ?d = make (Some "float") ?d
  let bool ?d = make (Some "bool") ?d
  let string ?d = make (Some "string") ?d
  let list ?d = make (Some "list") ?d

  (* Harmful function, do not use *)
  let force_type c (t : ut) : 'a t =
    match t#kind with
      | Some x when x = c -> (Obj.magic t : 'a t)
      | _ -> raise (Mismatch t)

  let as_unit t : unit t = force_type "unit" t
  let as_int t : int t = force_type "int" t
  let as_float t : float t = force_type "float" t
  let as_bool t : bool t = force_type "bool" t
  let as_string t : string t = force_type "string" t
  let as_list t : string list t = force_type "list" t
  let path_of_string p = Str.split path_sep_regexp p
  let string_of_path p = String.concat "." p

  let get_string (t : ut) =
    try
      match t#kind with
        | None -> None
        | Some "unit" -> Some ""
        | Some "int" -> Some (string_of_int (as_int t)#get)
        | Some "float" -> Some (string_of_float (as_float t)#get)
        | Some "bool" -> Some (string_of_bool (as_bool t)#get)
        | Some "string" -> Some (as_string t)#get
        | Some "list" -> Some (String.concat ":" (as_list t)#get)
        | _ -> assert false
    with Undefined _ -> None

  let get_d_string (t : ut) =
    let mapopt f = function None -> None | Some x -> Some (f x) in
    try
      match t#kind with
        | None -> None
        | Some "unit" -> mapopt (fun () -> "") (as_unit t)#get_d
        | Some "int" -> mapopt string_of_int (as_int t)#get_d
        | Some "float" -> mapopt string_of_float (as_float t)#get_d
        | Some "bool" -> mapopt string_of_bool (as_bool t)#get_d
        | Some "string" -> (as_string t)#get_d
        | Some "list" -> mapopt (String.concat ":") (as_list t)#get_d
        | _ -> assert false
    with Undefined _ -> None

  let descr ?(prefix = []) (t : ut) =
    let rec aux prefix t =
      let p s = if prefix = "" then s else prefix ^ "." ^ s in
      let subs = List.map (function s -> aux (p s) (t#path [s])) t#subs in
      Printf.sprintf "## %s\n" t#descr
      ^ begin match get_d_string t with
        | None -> ""
        | Some d -> Printf.sprintf "# default :%s\n" d
      end
      ^ begin match (t#kind, get_string t) with
        | Some k, None -> Printf.sprintf "#%s\t%-30s\n" k prefix
        | Some k, Some p -> Printf.sprintf "%s\t%-30s :%s\n" k prefix p
        | _ -> ""
      end
      ^ begin match t#comments with
        | [] -> ""
        | l ->
            "# comments:\n"
            ^ String.concat ""
                (List.map (fun s -> Printf.sprintf "#  %s\n" s) l)
      end
      ^ "\n" ^ String.concat "" subs
    in
    aux (string_of_path prefix) (t#path prefix)

  let dump ?(prefix = []) (t : ut) =
    let rec aux prefix t =
      let p s = if prefix = "" then s else prefix ^ "." ^ s in
      let subs = List.map (function s -> aux (p s) (t#path [s])) t#subs in
      begin match t#kind with
        | Some k -> (
            match (get_d_string t, get_string t) with
              | None, None -> Printf.sprintf "#%s\t%-30s\n" k prefix
              | Some p, None -> Printf.sprintf "#%s\t%-30s :%s\n" k prefix p
              | Some p, Some p' when p' = p ->
                  Printf.sprintf "#%s\t%-30s :%s\n" k prefix p
              | _, Some p -> Printf.sprintf "%s\t%-30s :%s\n" k prefix p)
        | _ -> ""
      end
      ^ String.concat "" subs
    in
    aux (string_of_path prefix) (t#path prefix)

  let conf_set (t : ut) s =
    if Str.string_match line_regexp s 0 then (
      let val0 = Str.matched_group 1 s in
      let val1 = Str.matched_group 2 s in
      let val2 = Str.matched_group 3 s in
      let st = t#path (path_of_string val1) in
      match val0 with
        | "unit" -> (
            match val2 = "" with
              | false -> raise (Wrong_Conf (s, "unit expected"))
              | true -> (as_unit st)#set ())
        | "int" ->
            let i =
              try int_of_string val2
              with Invalid_argument _ ->
                raise (Wrong_Conf (s, "integer expected"))
            in
            (as_int st)#set i
        | "float" ->
            let f =
              try float_of_string val2
              with Invalid_argument _ ->
                raise (Wrong_Conf (s, "float expected"))
            in
            (as_float st)#set f
        | "bool" ->
            let b =
              try bool_of_string val2
              with Invalid_argument _ ->
                raise (Wrong_Conf (s, "boolean expected"))
            in
            (as_bool st)#set b
        | "string" ->
            let s = val2 in
            (as_string st)#set s
        | "list" ->
            let l = Str.split list_sep_regexp val2 in
            (as_list st)#set l
        | _ -> raise (Wrong_Conf (s, "unknown type")))
    else raise (Wrong_Conf (s, "syntax error"))

  let conf_file t s =
    let nb = ref 0 in
    let f = open_in s in
    try
      while true do
        nb := !nb + 1;
        let l = input_line f in
        if Str.string_match comment_regexp l 0 then ()
        else begin
          try conf_set t l
          with Wrong_Conf (_, y) -> raise (File_Wrong_Conf (s, !nb, y))
        end
      done
    with End_of_file -> ()

  let args t =
    [
      ( ["--conf-file"; "-f"],
        Arg.String (conf_file t),
        "read the given configuration file" );
      ( ["--conf-set"; "-s"],
        Arg.String (conf_set t),
        "apply the given configuration assignation" );
      ( ["--conf-descr-key"],
        Arg.String
          (fun p ->
            Printf.printf "%s" (descr ~prefix:(path_of_string p) t);
            exit 0),
        "describe a configuration key" );
      ( ["--conf-descr"],
        Arg.Unit
          (fun () ->
            Printf.printf "%s" (descr t);
            exit 0),
        "display a described table of the configuration keys" );
      ( ["--conf-dump"],
        Arg.Unit
          (fun () ->
            Printf.printf "%s" (dump t);
            exit 0),
        "dump the configuration state" );
    ]
end

module Init = struct
  let conf = Conf.void "initialization configuration"

  (* Unix.fork is not implemented in Win32. *)
  let daemon_conf =
    if Sys.os_type <> "Win32" then conf else Conf.void "dummy conf"

  let conf_daemon =
    Conf.bool ~p:(daemon_conf#plug "daemon") ~d:false "run in daemon mode"

  let conf_daemon_pidfile =
    Conf.bool
      ~p:(conf_daemon#plug "pidfile")
      ~d:false "support for pidfile generation"

  let conf_daemon_pidfile_path =
    Conf.string ~p:(conf_daemon_pidfile#plug "path") "path to pidfile"

  let conf_daemon_pidfile_perms =
    Conf.int ~d:0o640
      ~p:(conf_daemon_pidfile#plug "perms")
      "Unix file permissions for pidfile. Default: `0o640`."

  let conf_daemon_drop_user =
    Conf.bool
      ~p:(conf_daemon#plug "change_user")
      ~d:false "Changes the effective user (drops privileges)."

  let conf_daemon_user =
    Conf.string
      ~p:(conf_daemon_drop_user#plug "user")
      ~d:"daemon" "User used to run the daemon."

  let conf_daemon_group =
    Conf.string
      ~p:(conf_daemon_drop_user#plug "group")
      ~d:"daemon" "Group used to run the daemon."

  let conf_trace =
    Conf.bool ~p:(conf#plug "trace") ~d:false "dump an initialization trace"

  let conf_catch_exn =
    Conf.bool ~p:(conf#plug "catch_exn") ~d:true
      "catch exceptions, use false to backtrace exceptions"

  type t = {
    name : string;
    mutable launched : bool;
    mutable depends : t list;
    mutable triggers : t list;
    mutable mutex : Mutex.t;
    f : unit -> unit;
  }

  let make ?(name = "") ?(depends = []) ?(triggers = []) ?(after = [])
      ?(before = []) f =
    let na =
      { name; launched = false; depends; triggers; mutex = Mutex.create (); f }
    in
    List.iter (fun a -> a.triggers <- na :: a.triggers) after;
    List.iter (fun a -> a.depends <- na :: a.depends) before;
    na

  let start = make ~name:"init-start" flush_all
  let stop = make ~name:"init-stop" flush_all

  let at_start ?name ?depends ?triggers ?after ?before f =
    let a = make ?name ?depends ?triggers ?after ?before f in
    start.triggers <- a :: start.triggers;
    a

  let at_stop ?name ?depends ?triggers ?after ?before f =
    let a = make ?name ?depends ?triggers ?after ?before f in
    stop.depends <- a :: stop.depends;
    a

  let rec exec a =
    let log =
      if conf_trace#get then fun s ->
        let id = Thread.id (Thread.self ()) in
        Printf.printf "init(%i):%-35s@%s\n%!" id a.name s
      else fun _ -> ()
    in
    log "called";
    Mutex.lock a.mutex;
    try
      if not a.launched then begin
        a.launched <- true;
        log "start";
        log "start-depends";
        List.iter exec a.depends;
        log "stop-depends";
        log "start-atom";
        a.f ();
        log "stop-atom";
        log "start-triggers";
        List.iter exec a.triggers;
        log "stop-triggers";
        log "stop"
      end;
      Mutex.unlock a.mutex;
      log "return"
    with e ->
      Mutex.unlock a.mutex;
      raise e

  let rec wait_signal () =
    try ignore (Thread.wait_signal [Sys.sigterm; Sys.sigint]) with
      | Unix.Unix_error (Unix.EINTR, _, _) -> ()
      | Sys_error s when s = "Thread.wait_signal: Interrupted system call" ->
          wait_signal ()

  exception StartError of exn
  exception StopError of exn

  (* Dummy functions in the case where
   * Printexc does not have the required
   * functions. *)
  let get_backtrace () =
    "ocaml-dtools not compiled with ocaml >= 3.11, cannot print stack backtrace"

  (* For the compiler.. *)
  let () = ignore (get_backtrace ())

  open Printexc

  let main f () =
    begin try exec start with e -> raise (StartError e)
    end;
    let quit pid = if Sys.os_type <> "Win32" then Unix.kill pid Sys.sigterm in
    let thread pid =
      try
        f ();
        quit pid
      with e ->
        let se = Printexc.to_string e in
        Printf.eprintf
          "init: exception encountered during main phase:\n  %s\n%!" se;
        Printf.eprintf "exception: %s\n%s%!" se (get_backtrace ());
        if conf_catch_exn#get then quit pid else raise e
    in
    let th = Thread.create thread (Unix.getpid ()) in
    if Sys.os_type <> "Win32" then wait_signal () else Thread.join th;
    try exec stop with e -> raise (StopError e)

  let catch f clean =
    try
      f ();
      clean ()
    with
      | StartError e ->
          Printf.eprintf
            "init: exception encountered during start phase:\n  %s\n%!"
            (Printexc.to_string e);
          clean ();
          exit (-1)
      | StopError e ->
          Printf.eprintf
            "init: exception encountered during stop phase:\n  %s\n%!"
            (Printexc.to_string e);
          clean ();
          exit (-1)

  (** A function to reopen a file descriptor * Thanks to Xavier Leroy! * Ref:
      http://caml.inria.fr/pub/ml-archives/caml-list/2000/01/ *
      a7e3bbdfaab33603320d75dbdcd40c37.en.html *)
  let reopen_out outchan filename =
    flush outchan;
    let fd1 = Unix.descr_of_out_channel outchan in
    let fd2 = Unix.openfile filename [Unix.O_WRONLY] 0o666 in
    Unix.dup2 fd2 fd1;
    Unix.close fd2

  (** The same for inchan *)
  let reopen_in inchan filename =
    let fd1 = Unix.descr_of_in_channel inchan in
    let fd2 = Unix.openfile filename [Unix.O_RDONLY] 0o666 in
    Unix.dup2 fd2 fd1;
    Unix.close fd2

  let daemonize () =
    if Unix.fork () <> 0 then exit 0;
    (* Detach from the console *)
    if Unix.setsid () < 0 then exit 1;
    (* Refork.. *)
    if Unix.fork () <> 0 then exit 0;
    (* Change umask to 0 *)
    ignore (Unix.umask 0);
    (* chdir to / *)
    Unix.chdir "/";
    if conf_daemon_pidfile#get then begin
      (* Write PID to file *)
      let filename = conf_daemon_pidfile_path#get in
      let f =
        open_out_gen
          [Open_wronly; Open_creat; Open_trunc]
          conf_daemon_pidfile_perms#get filename
      in
      let pid = Unix.getpid () in
      output_string f (string_of_int pid);
      output_char f '\n';
      close_out f
    end;
    (* Reopen usual file descriptor *)
    reopen_in stdin "/dev/null";
    reopen_out stdout "/dev/null";
    reopen_out stderr "/dev/null"

  let cleanup_daemon () =
    if conf_daemon_pidfile#get then (
      try
        let filename = conf_daemon_pidfile_path#get in
        Sys.remove filename
      with _ -> ())

  exception Root_prohibited of [ `User | `Group | `Both ]

  let exit_when_root () =
    (* Change user.. *)
    if conf_daemon_drop_user#get then begin
      let grd = Unix.getgrnam conf_daemon_group#get in
      let gid = grd.Unix.gr_gid in
      if Unix.getegid () <> gid then Unix.setgid gid;
      let pwd = Unix.getpwnam conf_daemon_user#get in
      let uid = pwd.Unix.pw_uid in
      if Unix.geteuid () <> uid then Unix.setuid uid
    end;
    match (Unix.geteuid (), Unix.getegid ()) with
      | 0, 0 -> raise (Root_prohibited `Both)
      | 0, _ -> raise (Root_prohibited `User)
      | _, 0 -> raise (Root_prohibited `Group)
      | _ -> ()

  let init ?(prohibit_root = false) f =
    if prohibit_root then exit_when_root ();
    if conf_daemon#get && Sys.os_type <> "Win32" then daemonize ();
    let signal_h _ = () in
    Sys.set_signal Sys.sigterm (Sys.Signal_handle signal_h);
    Sys.set_signal Sys.sigint (Sys.Signal_handle signal_h);
    (* We block signals that would kill us,
     * we'll wait for them and shutdown cleanly.
     * On Windows this is impossible so the only way for the application
     * to shutdown is to terminate the main function [f]. *)
    if Sys.os_type <> "Win32" then
      ignore (Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigterm; Sys.sigint]);
    let cleanup =
      if conf_daemon#get && Sys.os_type <> "Win32" then cleanup_daemon
      else fun () -> ()
    in
    catch (main f) cleanup

  let args =
    if Sys.os_type <> "Win32" then
      [
        ( ["-d"; "--daemon"],
          Arg.Unit (fun () -> conf_daemon#set true),
          "Run in daemon mode." );
      ]
    else []
end

module Log = struct
  type entry = {
    time : float;
    label : string option;
    level : int option;
    log : string;
  }

  type pending_entry = { colorize : entry -> entry; entry : entry }

  type t =
    < active : int -> bool
    ; level : int
    ; set_level : int -> unit
    ; path : Conf.path
    ; f : 'a. int -> ('a, unit, string, unit) format4 -> 'a
    ; g :
        'a.
        ?colorize:(entry -> entry) ->
        int ->
        ('a, unit, string, unit) format4 ->
        'a >

  type custom_log = { timestamp : bool; exec : string -> unit }

  let log_ch = ref None

  (* Custom logging methods. *)
  let custom_log : (string, custom_log) Hashtbl.t = Hashtbl.create 0
  let add_custom_log name f = Hashtbl.replace custom_log name f
  let rm_custom_log name = Hashtbl.remove custom_log name
  let conf = Conf.void "log configuration"
  let conf_level = Conf.int ~p:(conf#plug "level") ~d:3 "general log level"

  let conf_unix_timestamps =
    Conf.bool
      ~p:(conf#plug "unix_timestamps")
      ~d:false
      "display unix timestamps (subsecond accuracy, timezone independent)"

  let conf_file = Conf.bool ~p:(conf#plug "file") ~d:true "log to file"
  let conf_file_path = Conf.string ~p:(conf_file#plug "path") "path to log file"

  let conf_file_append =
    Conf.bool ~p:(conf_file#plug "append") ~d:true "append log to the file"

  let conf_file_perms =
    Conf.int ~p:(conf_file#plug "perms") ~d:0o600 "log file permissions"

  let conf_stdout = Conf.bool ~p:(conf#plug "stdout") ~d:false "log to stdout"

  let timestamp time =
    match conf_unix_timestamps#get with
      | true -> Printf.sprintf "%f" time
      | false ->
          let date = Unix.localtime time in
          Printf.sprintf "%d/%02d/%02d %02d:%02d:%02d"
            (date.Unix.tm_year + 1900) (date.Unix.tm_mon + 1) date.Unix.tm_mday
            date.Unix.tm_hour date.Unix.tm_min date.Unix.tm_sec

  let message ?(show_timestamp = true) { time; label; level; log } =
    let label =
      match (label, level) with
        | None, None -> ""
        | Some l, None -> Printf.sprintf "[%s] " l
        | None, Some d -> Printf.sprintf "[%d] " d
        | Some l, Some d -> Printf.sprintf "[%s:%d] " l d
    in
    let str = label ^ log in
    let timestamp = if show_timestamp then timestamp time ^ " " else "" in
    Printf.sprintf "%s%s" timestamp str

  let print { colorize; entry } =
    let to_stdout = conf_stdout#get in
    let to_file = !log_ch <> None in
    begin match to_stdout || to_file with
      | true ->
          let do_stdout () =
            Printf.printf "%s\n%!" (message (colorize entry))
          in
          let do_file () =
            match !log_ch with
              | None -> ()
              | Some ch -> Printf.fprintf ch "%s\n%!" (message entry)
          in
          if to_stdout then do_stdout ();
          if to_file then do_file ()
      | false -> ()
    end;
    let f _ x = x.exec (message ~show_timestamp:x.timestamp entry) in
    Hashtbl.iter f custom_log

  (* Avoid interlacing logs *)
  let log_mutex = Mutex.create ()
  let log_condition = Condition.create ()
  let log_queue = ref (Queue.create ())
  let log_stop = ref false
  let log_thread = ref None

  let mutexify f x =
    Mutex.lock log_mutex;
    try
      let ret = f x in
      Mutex.unlock log_mutex;
      ret
    with e ->
      Mutex.unlock log_mutex;
      raise e

  let rotate_queue () =
    let new_q = Queue.create () in
    mutexify
      (fun () ->
        let q = !log_queue in
        log_queue := new_q;
        q)
      ()

  let flush_queue () =
    let rec flush q =
      Queue.iter print q;
      let q = rotate_queue () in
      if not (Queue.is_empty q) then flush q
    in
    flush (rotate_queue ())

  let flush () = flush_queue ()

  let log_thread_fn () =
    let rec f () =
      flush_queue ();
      let log_stop =
        mutexify
          (fun () ->
            if !log_stop then true
            else begin
              Condition.wait log_condition log_mutex;
              !log_stop
            end)
          ()
      in
      if not log_stop then f ()
    in
    f ()

  let proceed =
    mutexify (fun entry ->
        Queue.push entry !log_queue;
        Condition.signal log_condition)

  let make path : t =
    let path_str = Conf.string_of_path path in
    let conf_level = ref (fun () -> conf_level#get) in
    object (self : t)
      method path = path
      method active level = level <= !conf_level ()
      method level = !conf_level ()
      method set_level level = conf_level := fun () -> level

      method g ?(colorize = fun x -> x) level =
        match self#active level with
          | true ->
              let time = Unix.gettimeofday () in
              Printf.ksprintf (fun s ->
                  List.iter
                    (fun log ->
                      proceed
                        {
                          colorize;
                          entry =
                            {
                              time;
                              label = Some path_str;
                              level = Some level;
                              log;
                            };
                        })
                    (String.split_on_char '\n' s))
          | false -> Printf.ksprintf (fun _ -> ())

      method f level = self#g ?colorize:None level
    end

  let init () =
    let time = Unix.gettimeofday () in
    let reopen_log =
      if conf_file#get then begin
        let opts =
          [Open_wronly; Open_creat; Open_nonblock]
          @ if conf_file_append#get then [Open_append] else [Open_trunc]
        in
        let log_file_path = conf_file_path#get in
        let log_file_perms = conf_file_perms#get in
        log_ch := Some (open_out_gen opts log_file_perms log_file_path);
        fun _ ->
          begin match !log_ch with
            | None -> ()
            | Some ch ->
                log_ch := None;
                close_out ch
          end;
          log_ch := Some (open_out_gen opts log_file_perms log_file_path)
      end
      else fun _ -> ()
    in
    (* Re-open log file on SIGUSR1 -- for logrotate *)
    if Sys.os_type <> "Win32" then
      Sys.set_signal Sys.sigusr1 (Sys.Signal_handle reopen_log);
    print
      {
        colorize = (fun x -> x);
        entry = { time; level = None; label = None; log = ">>> LOG START" };
      };
    log_thread := Some (Thread.create log_thread_fn ())

  let start = Init.make ~name:"init-log-start" ~before:[Init.start] init

  let close () =
    let time = Unix.gettimeofday () in
    mutexify (fun () -> log_stop := true) ();
    proceed
      {
        colorize = (fun x -> x);
        entry = { time; level = None; label = None; log = ">>> LOG END" };
      };
    begin match !log_thread with
      | None -> ()
      | Some th ->
          log_thread := None;
          Condition.signal log_condition;
          Thread.join th
    end;
    match !log_ch with
      | None -> ()
      | Some ch ->
          log_ch := None;
          close_out ch

  let stop = Init.make ~name:"init-log-stop" ~after:[Init.stop] close

  let args =
    [
      ( ["--log-stdout"],
        Arg.Unit (fun () -> conf_stdout#set true),
        "log also to stdout" );
      ( ["--log-file"; "-l"],
        Arg.String
          (fun s ->
            conf_file#set true;
            conf_file_path#set s),
        "log file" );
    ]
end
