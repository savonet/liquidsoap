(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2017 Savonet team

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

type _t = {
  stdin:    out_channel;
  stderr:   in_channel;
  stdout:   in_channel;
  mutable stopped: bool
}

type t = {
  out_pipe: Unix.file_descr;
  in_pipe:  Unix.file_descr;
  mutex:    Mutex.t;
  mutable process: _t option
}

type continuation = [
  | `Continue
  | `Stop
  | `Kill
  | `Delay of float
]

type 'a callback = 'a -> continuation

type pull = string -> int -> int -> int

type push = string -> int -> int -> int

type status = [
  | `Exception of exn
  | `Status of Unix.process_status
]

exception Finished

let get_process {process;_} =
  match process with
    | Some process -> process
    | None -> raise Finished

let stop t = Tutils.mutexify t.mutex (fun () ->
  if t.process = None then raise Finished;
  ignore(Unix.write t.in_pipe "0" 0 1)) ()

let kill t = Tutils.mutexify t.mutex (fun () ->
  if t.process = None then raise Finished;
  ignore(Unix.write t.in_pipe "1" 0 1)) ()

let send_stop ~log t = Tutils.mutexify t.mutex (fun () ->
  let process = get_process t in
  if not process.stopped then begin
    log "Closing process's stdin";
    process.stopped <- true;
    try close_out process.stdin with _ -> ()
  end) ()

let _kill = function
  | Some {stdout;stdin;stderr;_} ->
      ignore(Unix.close_process_full (stdout,stdin,stderr))
  | None -> ()

let cleanup ~log t = Tutils.mutexify t.mutex (fun () ->
  log "Cleaning up process";
  let {in_pipe;out_pipe;process;_} = t in
  t.process <- None;
  let kill () = _kill process in
  let silent f = try f () with _ -> () in
  List.iter silent [(fun () -> Unix.close in_pipe);
    (fun () -> Unix.close out_pipe); kill]) ()

let pusher fd buf ofs len =
  Unix.write fd buf ofs len

let puller in_pipe fd buf ofs len =
  let ret = Unix.read fd buf ofs len in
  if ret = 0 then ignore(Unix.write in_pipe "2" 0 1);
  ret

let run ?priority ?env ?on_start ?on_stdin ?on_stdout ?on_stderr ?on_stop ?log command =
    let with_default default = function
      | None -> default
      | Some v -> v
    in
    let priority =
      with_default Tutils.Non_blocking priority
    in
    let env =
      with_default (Unix.environment ()) env
    in
    let log =
      with_default (fun _ -> ()) log
    in
    let on_start =
      with_default (fun _ -> `Continue) on_start
    in
    let on_stop =
      with_default (fun _ -> false) on_stop
    in
    let on_stdout =
      with_default (fun _ -> `Continue) on_stdout
    in
    let on_stderr =
      with_default (fun _ -> `Continue) on_stderr
    in
    let on_stdin =
      with_default (fun _ -> `Continue) on_stdin
    in
    let create () =
      log "Starting process";
      let stdout,stdin,stderr =
        Unix.open_process_full command env
      in
      {stdin;stdout;stderr;stopped=false}
    in
    let process = create () in
    let out_pipe,in_pipe = Unix.pipe () in
    let mutex = Mutex.create () in
    let t = {in_pipe;out_pipe;mutex;process=Some process} in
    let create = Tutils.mutexify t.mutex (fun () ->
      _kill t.process;
      t.process <- Some (create ()))
    in
    let get_task handler decision =
      let process = get_process t in
      let stdout =
        Unix.descr_of_in_channel process.stdout
      in
      let stderr =
        Unix.descr_of_in_channel process.stderr
      in
      let read_events =
        [`Read out_pipe;`Read stdout;`Read stderr]
      in
      let continue_events =
        if process.stopped then read_events else
          (`Write (Unix.descr_of_out_channel process.stdin))::read_events
      in
      let events = match decision with
        | `Kill -> cleanup ~log t; []
        | `Stop -> send_stop ~log t; read_events
        | `Continue -> continue_events
        | `Delay d -> [`Delay d; `Read out_pipe]
      in
      { Duppy.Task.
          priority = priority;
          events   = events;
          handler  = handler
      }
    in
    let restart_decision handler = function
      | true ->
          create ();
          let fd = Unix.descr_of_out_channel (get_process t).stdin in 
          [get_task handler (on_start (pusher fd))]
      | false -> cleanup ~log t; []
    in
    let on_pipe () =
      let buf = " " in
      let ret = Unix.read out_pipe buf 0 1 in
      if ret <> 1 then assert false;
      match buf with
        | "0" -> `Stop 
        | "1" -> `Kill
        | "2" -> raise Finished 
        | _ -> assert false
    in
    let rec handler l =
      let process = get_process t in
      let get_task decision =
        [get_task handler decision]
      in
      let restart_decision =
        restart_decision handler
      in
      let stdout =
        Unix.descr_of_in_channel process.stdout
      in
      try
        let decision =
          if List.mem (`Read out_pipe) l then on_pipe () else
            let decisions = List.fold_left (fun cur -> function
              | `Read fd when fd = stdout -> (on_stdout (puller t.in_pipe fd))::cur
              | `Read fd -> (on_stderr (puller t.in_pipe fd))::cur
              | `Write fd -> (on_stdin (pusher fd))::cur
              | `Delay _ -> cur) [] l
            in
            List.fold_left (fun (cur:continuation) decision ->
                match decision, cur with
                  | `Kill, _
                  |  _, `Kill -> `Kill
                  | `Stop, _
                  | _, `Stop -> `Stop
                  | `Continue, `Continue -> `Continue
                  | `Delay d, `Delay d' -> `Delay (max d d')
                  | `Delay d, _ 
                  | _, `Delay d -> `Delay d) (`Continue) decisions
        in
        get_task decision
      with 
        | Finished ->
            log "Process exited";
            let status =
              let {stdout;stdin;stderr} = get_process t in
              `Status (Unix.close_process_full (stdout,stdin,stderr))
            in
            restart_decision (on_stop status)
        | e ->
            log (Printf.sprintf "Error while running process: %s\n%s"
              (Printexc.to_string e)
              (Printexc.get_backtrace ()));
            restart_decision (on_stop (`Exception e))
    in
    let fd = Unix.descr_of_out_channel (get_process t).stdin in
    Duppy.Task.add Tutils.scheduler (get_task handler (on_start (pusher fd)));
    t

let read buflen pull =
  let buf = Bytes.create buflen in
  let ret = pull buf 0 buflen in
  String.sub buf 0 ret 

let write data push =
  let len = String.length data in
  let rec f pos =
    if pos < len then
      f (pos+(push data pos (len-pos)))
  in
  f 0

let on_stdout t = Tutils.mutexify t.mutex (fun fn ->
  let fd = Unix.descr_of_in_channel (get_process t).stdout in
  fn (puller t.in_pipe fd))

let on_stdin t = Tutils.mutexify t.mutex (fun fn ->
  match t.process with
    | Some process ->
        if process.stopped then raise Finished;
        let fd = Unix.descr_of_out_channel process.stdin in
        fn (pusher fd)
    | None -> raise Finished)

let on_stderr t = Tutils.mutexify t.mutex (fun fn ->
  let fd = Unix.descr_of_in_channel (get_process t).stderr in
  fn (puller t.in_pipe fd))

let stopped t = Tutils.mutexify t.mutex (fun () ->
  try
    (get_process t).stopped
  with Finished -> true) ()
