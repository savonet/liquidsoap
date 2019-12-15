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

type process = { stdin : out_channel; stdout : in_channel; stderr : in_channel }

let open_process cmd env =
  let stdout, stdin, stderr = Unix.open_process_full cmd env in
  { stdin; stdout; stderr }

let close_process { stdout; stdin; stderr } =
  try Unix.close_process_full (stdout, stdin, stderr)
  with Unix.Unix_error (Unix.ECHILD, _, _) -> Unix.WEXITED 0

let wait { stdout; stdin; stderr } =
  let pid = Unix.process_full_pid (stdout, stdin, stderr) in
  try Unix.waitpid [] pid
  with Unix.Unix_error (Unix.ECHILD, _, _) -> (pid, Unix.WEXITED 0)

type _t = {
  in_pipe : Unix.file_descr;
  out_pipe : Unix.file_descr;
  p : process;
  mutable priority : Tutils.priority;
  mutable status : Unix.process_status option;
  mutable stopped : bool;
}

type t = { mutex : Mutex.t; mutable process : _t option }

type continuation =
  [ `Continue
  | `Stop
  | `Kill
  | `Delay of float
  | `Reschedule of Tutils.priority ]

type 'a callback = 'a -> continuation
type pull = Bytes.t -> int -> int -> int
type push = Bytes.t -> int -> int -> int
type status = [ `Exception of exn | `Status of Unix.process_status ]

exception Finished

(* Used to wrap exception raised in callbacks. *)
exception Wrapped of exn

let get_process { process; _ } =
  match process with Some process -> process | None -> raise Finished

let set_priority t =
  Tutils.mutexify t.mutex (fun priority ->
      match t.process with
        | None -> raise Finished
        | Some p -> p.priority <- priority)

let stop_c, kill_c, done_c =
  let fn = Bytes.make 1 in
  (fn '0', fn '1', fn '2')

let stop t =
  Tutils.mutexify t.mutex
    (fun () ->
      match t.process with
        | None -> raise Finished
        | Some { in_pipe } -> ignore (Unix.write in_pipe stop_c 0 1))
    ()

let kill t =
  Tutils.mutexify t.mutex
    (fun () ->
      match t.process with
        | None -> raise Finished
        | Some { in_pipe } -> ignore (Unix.write in_pipe kill_c 0 1))
    ()

let send_stop ~log t =
  Tutils.mutexify t.mutex
    (fun () ->
      let process = get_process t in
      if not process.stopped then (
        log "Closing process's stdin";
        process.stopped <- true;
        try close_out process.p.stdin with _ -> () ))
    ()

let _kill = function
  | Some { p; in_pipe; out_pipe } ->
      let silent f = try f () with _ -> () in
      List.iter silent
        [
          (fun () -> Unix.close in_pipe);
          (fun () -> Unix.close out_pipe);
          (fun () -> ignore (close_process p));
        ]
  | None -> ()

let cleanup ~log t =
  Tutils.mutexify t.mutex
    (fun () ->
      log "Cleaning up process";
      let { process; _ } = t in
      t.process <- None;
      _kill process)
    ()

let pusher fd buf ofs len = Unix.write fd buf ofs len

let puller in_pipe fd buf ofs len =
  let ret = try Unix.read fd buf ofs len with _ when Sys.win32 -> 0 in
  if len > 0 && ret = 0 then ignore (Unix.write in_pipe done_c 0 1);
  ret

let run ?priority ?env ?on_start ?on_stdin ?on_stdout ?on_stderr ?on_stop ?log
    command =
  let with_default default = function None -> default | Some v -> v in
  let priority = with_default Tutils.Non_blocking priority in
  let env = with_default (Unix.environment ()) env in
  let log = with_default (fun _ -> ()) log in
  let on_start = with_default (fun _ -> `Continue) on_start in
  let on_stop = with_default (fun _ -> false) on_stop in
  let mutex = Mutex.create () in
  let create () =
    log "Starting process";
    let p = open_process command env in
    let out_pipe, in_pipe = Unix.pipe () in
    let process =
      { in_pipe; out_pipe; p; priority; stopped = false; status = None }
    in
    ignore
      (Thread.create
         (fun () ->
           try
             let _, status = wait p in
             Tutils.mutexify mutex
               (fun () ->
                 if process.status = None then (
                   process.status <- Some status;
                   ignore (Unix.write in_pipe done_c 0 1) ))
               ()
           with _ -> ())
         ());
    process
  in
  let process = create () in
  let t = { mutex; process = Some process } in
  let create =
    Tutils.mutexify t.mutex (fun () ->
        _kill t.process;
        t.process <- Some (create ()))
  in
  let get_task handler decision =
    let process = get_process t in
    let stdout = Unix.descr_of_in_channel process.p.stdout in
    let stderr = Unix.descr_of_in_channel process.p.stderr in
    let read_events =
      List.fold_left
        (fun cur (fd, callback) ->
          if callback <> None then `Read fd :: cur else cur)
        [`Read process.out_pipe]
        [(stdout, on_stdout); (stderr, on_stderr)]
    in
    let continue_events =
      if on_stdin <> None && not process.stopped then
        `Write (Unix.descr_of_out_channel process.p.stdin) :: read_events
      else read_events
    in
    let events =
      match decision with
        | `Kill ->
            cleanup ~log t;
            []
        | `Stop ->
            send_stop ~log t;
            read_events
        | `Reschedule p ->
            process.priority <- p;
            continue_events
        | `Continue -> continue_events
        | `Delay d -> [`Delay d; `Read process.out_pipe]
    in
    { Duppy.Task.priority = process.priority; events; handler }
  in
  let restart_decision handler = function
    | true ->
        create ();
        let fd = Unix.descr_of_out_channel (get_process t).p.stdin in
        [get_task handler (on_start (pusher fd))]
    | false ->
        cleanup ~log t;
        []
  in
  let on_pipe out_pipe =
    let buf = Bytes.make 1 ' ' in
    let ret = Unix.read out_pipe buf 0 1 in
    if ret <> 1 then assert false;
    match buf with
      | buf when buf = stop_c -> `Stop
      | buf when buf = kill_c -> `Kill
      | buf when buf = done_c -> raise Finished
      | _ -> assert false
  in
  let rec handler l =
    let process = get_process t in
    let get_task decision = [get_task handler decision] in
    let restart_decision = restart_decision handler in
    let stdout = Unix.descr_of_in_channel process.p.stdout in
    let wrap f x =
      try f x
      with exn ->
        (* We print backtrace here because the wrapping make us loose it afterward. *)
        log (Printexc.get_backtrace ());
        raise (Wrapped exn)
    in
    let on_stdout = wrap (with_default (fun _ -> `Continue) on_stdout) in
    let on_stderr = wrap (with_default (fun _ -> `Continue) on_stderr) in
    let on_stdin = wrap (with_default (fun _ -> `Continue) on_stdin) in
    try
      let decision =
        if List.mem (`Read process.out_pipe) l then on_pipe process.out_pipe
        else (
          let decisions =
            List.fold_left
              (fun cur -> function
                | `Read fd when fd = stdout ->
                    on_stdout (puller process.in_pipe fd) :: cur
                | `Read fd -> on_stderr (puller process.in_pipe fd) :: cur
                | `Write fd -> on_stdin (pusher fd) :: cur | `Delay _ -> cur)
              [] l
          in
          List.fold_left
            (fun cur decision ->
              match (decision, cur) with
                | `Kill, _ | _, `Kill -> `Kill
                | `Stop, _ | _, `Stop -> `Stop
                | `Reschedule p, cur ->
                    process.priority <- p;
                    cur
                | `Continue, `Continue -> `Continue
                | `Delay d, `Delay d' -> `Delay (max d d')
                | `Delay d, _ | _, `Delay d -> `Delay d)
            `Continue decisions )
      in
      get_task decision
    with
      | Finished ->
          let { in_pipe; out_pipe; p; status } = get_process t in
          let silent f arg = try ignore (f arg) with _ -> () in
          silent Unix.close in_pipe;
          silent Unix.close out_pipe;
          let status =
            match status with
              | Some status ->
                  (* Issue #865: if status is known, we still need
                        to call Unix.close_process_full to cleanup the
                        process' pipes. *)
                  silent close_process p;
                  status
              | None -> close_process p
          in
          let descr =
            match status with
              | Unix.WEXITED c -> Printf.sprintf "Process exited with code %d" c
              | Unix.WSIGNALED s ->
                  Printf.sprintf "Process was killed by signal %d" s
              | Unix.WSTOPPED s ->
                  Printf.sprintf "Process was stopped by signal %d" s
          in
          log descr;
          t.process <- None;
          restart_decision (on_stop (`Status status))
      | e -> (
          let f e =
            log
              (Printf.sprintf "Error while running process: %s\n%s"
                 (Printexc.to_string e)
                 (Printexc.get_backtrace ()))
          in
          match e with
            | Wrapped e ->
                f e;
                raise e
            | _ ->
                f e;
                restart_decision (on_stop (`Exception e)) )
  in
  let fd = Unix.descr_of_out_channel (get_process t).p.stdin in
  Duppy.Task.add Tutils.scheduler (get_task handler (on_start (pusher fd)));
  t

let really_write ?(offset = 0) ?length data push =
  let length =
    match length with Some length -> length | None -> Bytes.length data
  in
  let rec f pos = if pos < length then f (pos + push data pos (length - pos)) in
  f offset

let on_stdout t fn =
  let process = Tutils.mutexify t.mutex (fun () -> get_process t) () in
  let fd = Unix.descr_of_in_channel process.p.stdout in
  fn (puller process.in_pipe fd)

let on_stdin t fn =
  let process =
    Tutils.mutexify t.mutex
      (fun () ->
        match t.process with
          | Some process ->
              if process.stopped then raise Finished;
              process
          | None -> raise Finished)
      ()
  in
  let fd = Unix.descr_of_out_channel process.p.stdin in
  fn (pusher fd)

let on_stderr t fn =
  let process = Tutils.mutexify t.mutex (fun () -> get_process t) () in
  let fd = Unix.descr_of_in_channel process.p.stderr in
  fn (puller process.in_pipe fd)

let stopped t =
  Tutils.mutexify t.mutex
    (fun () -> try (get_process t).stopped with Finished -> true)
    ()
