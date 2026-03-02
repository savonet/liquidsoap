(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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
type status = [ `Exception of exn | `Status of Unix.process_status ]

type _t = {
  in_pipe : Unix.file_descr;
  out_pipe : Unix.file_descr;
  p : process;
  status : status option Atomic.t;
  stopped : bool Atomic.t;
  priority : Tutils.priority Atomic.t;
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

let open_process cmd env =
  let stdout, stdin, stderr = Unix.open_process_full cmd env in
  { stdin; stdout; stderr }

let close_process { stdout; stdin; stderr } =
  try `Status (Unix.close_process_full (stdout, stdin, stderr)) with
    | Unix.Unix_error (Unix.ECHILD, _, _) -> `Status (Unix.WEXITED 0)
    | exn -> `Exception exn

let wait { stdout; stdin; stderr } =
  let pid = Unix.process_full_pid (stdout, stdin, stderr) in
  try
    let pid, status = Unix.waitpid [] pid in
    (pid, `Status status)
  with
    | Unix.Unix_error (Unix.ECHILD, _, _) -> (pid, `Status (Unix.WEXITED 0))
    | exn -> (pid, `Exception exn)

exception Finished

(* Used to wrap exception raised in callbacks. *)
exception Wrapped of exn

(* Wrapper around Unix.read that automatically retries on EINTR.
   EINTR occurs when a signal is delivered during a blocking read,
   which is common in process handling where SIGCHLD signals are frequent. *)
let rec read fd buf ofs len =
  try Unix.read fd buf ofs len
  with Unix.Unix_error (Unix.EINTR, _, _) -> read fd buf ofs len

let get_process { process; _ } =
  match process with Some process -> process | None -> raise Finished

let set_priority t =
  Mutex_utils.mutexify t.mutex (fun priority ->
      match t.process with
        | None -> raise Finished
        | Some p -> Atomic.set p.priority priority)

let stop_c, kill_c, done_c =
  let fn = Bytes.make 1 in
  (fn '0', fn '1', fn '2')

let stop t =
  Mutex_utils.mutexify t.mutex
    (fun () ->
      match t.process with
        | None -> raise Finished
        | Some { in_pipe } -> (
            try ignore (Unix.write in_pipe stop_c 0 1) with _ -> ()))
    ()

let kill t =
  Mutex_utils.mutexify t.mutex
    (fun () ->
      match t.process with
        | None -> raise Finished
        | Some { in_pipe } -> (
            try ignore (Unix.write in_pipe kill_c 0 1) with _ -> ()))
    ()

let send_stop ~log t =
  Mutex_utils.mutexify t.mutex
    (fun () ->
      let process = get_process t in
      match Atomic.exchange process.stopped true with
        | false -> (
            log "Closing process's stdin";
            try close_out process.p.stdin with _ -> ())
        | _ -> ())
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
  Mutex_utils.mutexify t.mutex
    (fun () ->
      log "Cleaning up process";
      let { process; _ } = t in
      t.process <- None;
      _kill process)
    ()

let pusher fd buf ofs len = Unix.write fd buf ofs len
let puller fd buf ofs len = try read fd buf ofs len with _ when Sys.win32 -> 0

let run ?priority ?env ?on_start ?on_stdin ?on_stdout ?on_stderr ?on_stop ?log
    command =
  let with_default default = function None -> default | Some v -> v in
  let priority = with_default `Non_blocking priority in
  let env = with_default (Unix.environment ()) env in
  let log = with_default (fun _ -> ()) log in
  let on_start = with_default (fun _ -> `Continue) on_start in
  let on_stop = with_default (fun _ -> false) on_stop in
  let mutex = Mutex.create () in
  let create () =
    log "Starting process";
    let p = open_process command env in
    let out_pipe, in_pipe = Unix.pipe ~cloexec:true () in
    let process =
      {
        in_pipe;
        out_pipe;
        p;
        priority = Atomic.make priority;
        stopped = Atomic.make false;
        status = Atomic.make None;
      }
    in
    ignore
      (Thread.create
         (fun () ->
           try
             let _, status = wait p in
             Mutex_utils.mutexify mutex
               (fun () ->
                 if Atomic.compare_and_set process.status None (Some status)
                 then (
                   (try close_out p.stdin with _ -> ());
                   ignore (Unix.write in_pipe done_c 0 1)))
               ()
           with _ -> ())
         ());
    process
  in
  let process = create () in
  let t = { mutex; process = Some process } in
  let create =
    Mutex_utils.mutexify t.mutex (fun () ->
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
      if on_stdin <> None && not (Atomic.get process.stopped) then
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
            Atomic.set process.priority p;
            continue_events
        | `Continue -> continue_events
        | `Delay d -> [`Delay d; `Read process.out_pipe]
    in
    { Duppy.Task.priority = Atomic.get process.priority; events; handler }
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
  (* Read any remaining data from stdout/stderr pipes. Called when the process
     exits to ensure we don't miss any output. Reads until EOF on each pipe. *)
  let read_remaining_pipes () =
    let process = get_process t in
    let stdout = Unix.descr_of_in_channel process.p.stdout in
    let stderr = Unix.descr_of_in_channel process.p.stderr in
    let buf = Bytes.create 4096 in
    let drain_fd fd callback =
      let rec loop () =
        let n = try read fd buf 0 (Bytes.length buf) with _ -> 0 in
        if n > 0 then begin
          ignore
            (callback (fun b ofs len ->
                 let to_copy = min len n in
                 Bytes.blit buf 0 b ofs to_copy;
                 to_copy));
          loop ()
        end
      in
      loop ()
    in
    Option.iter (drain_fd stdout) on_stdout;
    Option.iter (drain_fd stderr) on_stderr
  in
  let on_pipe out_pipe =
    let buf = Bytes.make 1 ' ' in
    let ret = read out_pipe buf 0 1 in
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
        let bt = Printexc.get_raw_backtrace () in
        Printexc.raise_with_backtrace (Wrapped exn) bt
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
                | `Read fd when fd = stdout -> on_stdout (puller fd) :: cur
                | `Read fd -> on_stderr (puller fd) :: cur
                | `Write fd -> on_stdin (pusher fd) :: cur
                | `Delay _ -> cur)
              [] l
          in
          List.fold_left
            (fun cur decision ->
              match (decision, cur) with
                | `Kill, _ | _, `Kill -> `Kill
                | `Stop, _ | _, `Stop -> `Stop
                | `Reschedule p, cur ->
                    Atomic.set process.priority p;
                    cur
                | `Continue, `Continue -> `Continue
                | `Delay d, `Delay d' -> `Delay (max d d')
                | `Delay d, _ | _, `Delay d -> `Delay d)
            `Continue decisions)
      in
      get_task decision
    with
      | Finished ->
          read_remaining_pipes ();
          let { in_pipe; out_pipe; p; status } = get_process t in
          let silent f arg = try ignore (f arg) with _ -> () in
          silent Unix.close in_pipe;
          silent Unix.close out_pipe;
          let status =
            match Atomic.get status with
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
              | `Status (Unix.WEXITED c) ->
                  Printf.sprintf "Process exited with code %d" c
              | `Status (Unix.WSIGNALED s) ->
                  Printf.sprintf "Process was killed by signal %d" s
              | `Status (Unix.WSTOPPED s) ->
                  Printf.sprintf "Process was stopped by signal %d" s
              | `Exception exn ->
                  Printf.sprintf
                    "Exception %s was raised while stopping the process."
                    (Printexc.to_string exn)
          in
          log descr;
          t.process <- None;
          restart_decision (on_stop status)
      | e -> (
          let bt = Printexc.get_backtrace () in
          let f e =
            log
              (Printf.sprintf "Error while running process: %s\n%s"
                 (Printexc.to_string e) bt)
          in
          match e with
            | Wrapped e ->
                f e;
                raise e
            | _ ->
                f e;
                restart_decision (on_stop (`Exception e)))
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
  let process = Mutex_utils.mutexify t.mutex (fun () -> get_process t) () in
  let fd = Unix.descr_of_in_channel process.p.stdout in
  fn (puller fd)

let on_stdin t fn =
  let process =
    Mutex_utils.mutexify t.mutex
      (fun () ->
        match t.process with
          | Some process ->
              if Atomic.get process.stopped then raise Finished;
              process
          | None -> raise Finished)
      ()
  in
  let fd = Unix.descr_of_out_channel process.p.stdin in
  fn (pusher fd)

let on_stderr t fn =
  let process = Mutex_utils.mutexify t.mutex (fun () -> get_process t) () in
  let fd = Unix.descr_of_in_channel process.p.stderr in
  fn (puller fd)

let stopped t =
  Mutex_utils.mutexify t.mutex
    (fun () -> try Atomic.get (get_process t).stopped with Finished -> true)
    ()
