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

let log = Log.make ["process"]
let process = Modules.process

module Http = Liq_http

let _ =
  if Sys.os_type <> "Win32" then
    ignore
      (let ret_t =
         Lang.record_t [("user", Lang.float_t); ("system", Lang.float_t)]
       in
       Lang.add_builtin ~base:process "time" ~category:`System [] ret_t
         ~descr:"Get the execution time of the current liquidsoap process."
         (fun _ ->
           let { Unix.tms_utime = user; tms_stime = system } = Unix.times () in
           Lang.record
             [("user", Lang.float user); ("system", Lang.float system)]))

let () =
  List.iter
    (fun (name, fd) ->
      ignore
        (Lang.add_builtin_value ~base:process name ~category:`System
           ~descr:("The process' " ^ name)
           (Builtins_socket.Socket_value.to_value (Http.unix_socket ~pos:[] fd))
           Builtins_socket.Socket_value.t))
    [("stdin", Unix.stdin); ("stdout", Unix.stdout); ("stderr", Unix.stderr)]

let _ =
  let ret_t =
    Lang.method_t Lang.unit_t
      [
        ( "stdout",
          ([], Lang.string_t),
          "Messages written by process on standard output stream." );
        ( "stderr",
          ([], Lang.string_t),
          "Messages written by process on standard error stream." );
        ( "status",
          ( [],
            Lang.method_t Lang.string_t
              [
                ( "code",
                  ([], Lang.int_t),
                  "Returned status code (or signal, in case the process was \
                   killed or stopped by a signal)." );
                ( "description",
                  ([], Lang.string_t),
                  "Returned description (in case an exception was raised)." );
              ] ),
          "Status when process ended, can be one of `\"exit\"` (the program \
           exited, the `status` code is then relevant), `\"killed\"` (the \
           program was killed by signal given in `status` code), `\"stopped\"` \
           (the program was stopped by signal given in `status` code) or \
           `\"exception\"` (the program raised and exception detailed in the \
           `description`)." );
      ]
  in
  let env_t = Lang.product_t Lang.string_t Lang.string_t in
  let path_t = Lang.list_t Lang.string_t in
  Lang.add_builtin ~base:process "run" ~category:`System
    ~descr:
      "Run a process in a shell environment. Returns the standard output, as \
       well as standard error and status as methods. The status can be \
       \"exit\" (the status code is set), \"killed\" or \"stopped\" (the \
       status code is the signal), or \"exception\" (the description is set) \
       or \"timeout\" (the description is the run time)."
    [
      ("env", Lang.list_t env_t, Some (Lang.list []), Some "Process environment");
      ( "inherit_env",
        Lang.bool_t,
        Some (Lang.bool true),
        Some
          "Inherit calling process's environment when `env` parameter is empty."
      );
      ( "stdin",
        Lang.string_t,
        Some (Lang.string ""),
        Some "Data to write to the process' standard input." );
      ( "rwdirs",
        path_t,
        Some (Lang.list [Lang.string "default"]),
        Some
          "Read/write directories for sandboxing. `\"default\"` expands to \
           sandbox default." );
      ( "rodirs",
        path_t,
        Some (Lang.list [Lang.string "default"]),
        Some
          "Read-only directories for sandboxing `\"default\"` expands to \
           sandbox default." );
      ( "network",
        Lang.nullable_t Lang.bool_t,
        Some Lang.null,
        Some
          "Enable or disable network inside sandboxed environment (sandbox \
           default if not specified)." );
      ( "timeout",
        Lang.nullable_t Lang.float_t,
        Some Lang.null,
        Some "Cancel process after `timeout` has elapsed. Ignored if negative."
      );
      ("", Lang.string_t, None, Some "Command to run");
    ]
    ret_t
    (fun p ->
      let env = Lang.to_list (List.assoc "env" p) in
      let env =
        List.map
          (fun e ->
            let k, v = Lang.to_product e in
            (Lang.to_string k, Lang.to_string v))
          env
      in
      let stdin = Lang.to_string (List.assoc "stdin" p) in
      let sandbox_rw =
        List.map Lang.to_string (Lang.to_list (List.assoc "rwdirs" p))
      in
      let sandbox_rw =
        List.fold_left
          (fun cur el ->
            if el = "default" then cur @ Sandbox.conf_rw#get else el :: cur)
          [] sandbox_rw
      in
      let sandbox_ro =
        List.map Lang.to_string (Lang.to_list (List.assoc "rodirs" p))
      in
      let sandbox_ro =
        List.fold_left
          (fun cur el ->
            if el = "default" then cur @ Sandbox.conf_ro#get else el :: cur)
          [] sandbox_ro
      in
      let sandbox_network =
        match Lang.to_option (List.assoc "network" p) with
          | None -> Sandbox.conf_network#get
          | Some v -> Lang.to_bool v
      in
      let inherit_env = Lang.to_bool (List.assoc "inherit_env" p) in
      let env = if env = [] && inherit_env then Lang.environment () else env in
      let timeout =
        match
          Option.map Lang.to_float (Lang.to_option (List.assoc "timeout" p))
        with
          | Some f -> f
          | None -> -1.
      in
      let env = List.map (fun (k, v) -> Printf.sprintf "%s=%s" k v) env in
      let env = Array.of_list env in
      let cmd_value = Lang.to_string (List.assoc "" p) in
      let cmd =
        Sandbox.cmd ~rw:sandbox_rw ~ro:sandbox_ro ~network:sandbox_network
          cmd_value
      in
      let buflen = Utils.pagesize in
      let out_buf = Buffer.create buflen in
      let err_buf = Buffer.create buflen in
      let on_done (timed_out, status) =
        let stdout = Buffer.contents out_buf in
        let stderr = Buffer.contents err_buf in
        let status, code, description =
          match (timed_out, status) with
            | f, _ when 0. <= f -> ("timeout", -1, string_of_float f)
            | _, Some (`Exception e) -> ("exception", -1, Printexc.to_string e)
            | _, Some (`Status s) -> (
                match s with
                  | Unix.WEXITED c -> ("exit", c, "")
                  | Unix.WSIGNALED s -> ("killed", s, "")
                  | Unix.WSTOPPED s -> ("stopped", s, ""))
            | _ -> assert false
        in
        Lang.record
          [
            ("stdout", Lang.string stdout);
            ("stderr", Lang.string stderr);
            ( "status",
              Lang.meth (Lang.string status)
                [
                  ("code", Lang.int code);
                  ("description", Lang.string description);
                ] );
          ]
      in
      let synchronous () =
        let ((in_chan, out_ch, err_chan) as p) =
          Unix.open_process_full cmd env
        in
        if stdin <> "" then output_string out_ch stdin;
        close_out out_ch;
        let pull buf ch =
          let tmp = Bytes.create Utils.pagesize in
          let rec aux () =
            let n = input ch tmp 0 Utils.pagesize in
            if n = 0 then ()
            else (
              Buffer.add_subbytes buf tmp 0 n;
              aux ())
          in
          aux ()
        in
        pull out_buf in_chan;
        pull err_buf err_chan;
        (-1., Some (`Status (Unix.close_process_full p)))
      in
      let asynchronous () =
        let out_pipe, in_pipe = Unix.pipe ~cloexec:true () in
        Fun.protect
          ~finally:(fun () ->
            ignore (Unix.close in_pipe);
            ignore (Unix.close out_pipe))
          (fun () ->
            let pull buf fn =
              let bytes = Bytes.create buflen in
              let ret = fn bytes 0 buflen in
              Buffer.add_subbytes buf bytes 0 ret;
              `Continue
            in
            let on_stdout = pull out_buf in
            let on_stderr = pull err_buf in
            let status = ref None in
            let on_stop s =
              status := Some s;
              begin try ignore (Unix.write in_pipe (Bytes.of_string " ") 0 1)
              with _ -> ()
              end;
              false
            in
            let on_start _ = `Stop in
            let p =
              let log s = log#info "%s" s in
              Process_handler.run ~env ~on_start ~on_stop ~on_stdout ~on_stderr
                ~log cmd
            in
            let timed_out =
              try
                Tutils.wait_for (`Read out_pipe) timeout;
                -1.
              with
                | Tutils.Exit ->
                    (try Process_handler.kill p
                     with exn ->
                       log#important "Error while killing process: %s"
                         (Printexc.to_string exn));
                    status := Some (`Exception Tutils.Exit);
                    -1.
                | Tutils.Timeout f ->
                    (try Process_handler.kill p
                     with exn ->
                       log#important "Error while killing process: %s"
                         (Printexc.to_string exn));
                    f
            in
            (timed_out, !status))
      in
      let sync_run = not (Tutils.running ()) in
      if sync_run && 0. < timeout then
        log#important
          "Command %s cannot be executed with timeout %.02f because the \
           internal scheduler is not running. Most likely, this call is made \
           at the beginning of your script. We suggest that you wrap this call \
           in an asynchronous task using `thread.run`. If you really need this \
           value immediately as your script is starting, you should implement \
           the timeout within the process call itself."
          cmd_value timeout;
      on_done (if sync_run then synchronous () else asynchronous ()))

let process_quote =
  Lang.add_builtin ~base:process "quote" ~category:`System
    ~descr:
      "Return a quoted copy of the given string, suitable for use as one \
       argument in a command line, escaping all meta-characters. Warning: \
       under Windows, the output is only suitable for use with programs that \
       follow the standard Windows quoting conventions."
    [("", Lang.string_t, None, Some "String to escape")]
    Lang.string_t
    (fun p -> Lang.string (Filename.quote (Lang.to_string (List.assoc "" p))))

let _ =
  Lang.add_builtin ~base:process_quote "command" ~category:`System
    ~descr:
      "Return a quoted command line, suitable for use as an argument to \
       `process.run`.\n\n\
       The optional arguments `stdin`, `stdout` and `stderr` are file names \
       used to redirect the standard input, the standard output, or the \
       standard error of the command.\n\n\
       If `stdin=f` is given, a redirection `< f` is performed and the \
       standard input of the command reads from file `f`.\n\n\
       If `stdout=f` is given, a redirection `> f` is performed and the \
       standard output of the command is written to file `f`.\n\n\
       If `stderr=f` is given, a redirection `2> f` is performed and the \
       standard error of the command is written to file `f`.\n\n\
       If both `stdout=f` and `stderr=f` are given, with the exact same file \
       name `f`, a `2>&1` redirection is performed so that the standard output \
       and the standard error of the command are interleaved and redirected to \
       the same file `f`."
    [
      ( "stdin",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some "command standard input" );
      ( "stdout",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some "command standard output" );
      ( "stderr",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some "command standard error" );
      ( "args",
        Lang.list_t Lang.string_t,
        Some (Lang.list []),
        Some "command arguments" );
      ("", Lang.string_t, None, Some "Command to execute");
    ]
    Lang.string_t
    (fun p ->
      let stdin = Lang.to_valued_option Lang.to_string (List.assoc "stdin" p) in
      let stdout =
        Lang.to_valued_option Lang.to_string (List.assoc "stdout" p)
      in
      let stderr =
        Lang.to_valued_option Lang.to_string (List.assoc "stderr" p)
      in
      let args = List.map Lang.to_string (Lang.to_list (List.assoc "args" p)) in
      let cmd = Lang.to_string (List.assoc "" p) in
      Lang.string (Filename.quote_command cmd ?stdin ?stdout ?stderr args))
