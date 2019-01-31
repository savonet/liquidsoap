open Unix

type t = {
  pid:    int;
  stdin:  out_channel;
  stdout: in_channel;
  stderr: in_channel
}

(* Adapted from win32unix/unix.ml *)

let wait {pid;_} = Unix.waitpid [] pid

let open_process_cmdline prog args env =
  let (in_read, in_write) = pipe ~cloexec:true () in
  let (out_read, out_write) =
    try pipe ~cloexec:true ()
    with e -> close in_read; close in_write; raise e in
  let (err_read, err_write) =
    try pipe ~cloexec:true ()
    with e -> close in_read; close in_write;
              close out_read; close out_write; raise e in
  let stdin = out_channel_of_descr out_write in
  let stdout = in_channel_of_descr in_read in
  let stderr = in_channel_of_descr err_read in
  let pid = 
    begin
      try
        create_process_env prog args env
                           out_read in_write err_write
      with e ->
        close out_read; close out_write;
        close in_read; close in_write;
        close err_read; close err_write;
        raise e
    end
  in
  close out_read;
  close in_write;
  close err_write;
  {pid; stdin; stdout; stderr}

let open_process_shell fn cmd =
  let shell =
    try Sys.getenv "COMSPEC"
    with Not_found -> raise(Unix_error(ENOEXEC, "open_process_shell", cmd)) in
  fn shell [|shell;"/c ";cmd|]

let open_process cmd =
  open_process_shell open_process_cmdline cmd

let close_process {stdin; stdout; stderr; _} =
  close_out stdin; close_in stdout; close_in stderr;
