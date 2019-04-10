open Unix

type t = {
  pid:    int;
  stdin:  out_channel;
  stdout: in_channel;
  stderr: in_channel
}

(* Adapted from unix.ml *)

let shell = "/bin/sh"

let rec wait ({pid;_} as p) =
  try waitpid [] pid
  with Unix_error (EINTR, _, _) -> wait p

let rec file_descr_not_standard fd =
  let fd = Obj.magic fd in
  if fd >= 3 then fd else file_descr_not_standard (dup fd)

let safe_close fd =
  try close fd with Unix_error(_,_,_) -> ()

let perform_redirections new_stdin new_stdout new_stderr =
  let new_stdin = file_descr_not_standard new_stdin in
  let new_stdout = file_descr_not_standard new_stdout in
  let new_stderr = file_descr_not_standard new_stderr in
  (*  The three dup2 close the original stdin, stdout, stderr,
      which are the descriptors possibly left open
      by file_descr_not_standard *)
  dup2 new_stdin stdin;
  dup2 new_stdout stdout;
  dup2 new_stderr stderr;
  safe_close new_stdin;
  safe_close new_stdout;
  safe_close new_stderr

let open_proc prog args envopt input output error =
  match fork() with
    | 0 -> perform_redirections input output error;
      (match envopt with
         | Some env -> execve prog args env
         | None     -> execv prog args)
    | id -> id

let open_process_args prog args env =
  let (in_read, in_write) = pipe () in
  let (out_read, out_write) =
    try pipe ()
    with e -> close in_read; close in_write; raise e in
  let (err_read, err_write) =
    try pipe ()
    with e -> close in_read; close in_write;
              close out_read; close out_write; raise e in
  let stdin = out_channel_of_descr out_write in
  let stdout = in_channel_of_descr in_read in
  let stderr = in_channel_of_descr err_read in
  let pid =
    try
      open_proc prog args (Some env) out_read in_write err_write
    with e ->
      close out_read; close out_write;
      close in_read; close in_write;
      close err_read; close err_write;
      raise e
  in
  close out_read;
  close in_write;
  close err_write;
  {pid; stdin; stdout; stderr}

let open_process_shell fn cmd =
  fn shell [|shell; "-c"; cmd|]

let open_process cmd =
  open_process_shell open_process_args cmd

let close_process {stdin; stdout; stderr; _} =
  begin try close_out stdin with Sys_error _ -> () end;
  close_in stdout;
  close_in stderr
