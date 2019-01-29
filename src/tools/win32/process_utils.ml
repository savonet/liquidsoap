open Unix

(* Adapted from win32unix/unix.ml *)

let waitpid = Unix.waitpid []

let make_process_env env =
  Array.iter
    (fun s -> if String.contains s '\000' then raise(Unix_error(EINVAL, "", s)))
    env;
  String.concat "\000" (Array.to_list env) ^ "\000"

let open_process_cmdline prog args env =
  let (in_read, in_write) = pipe ~cloexec:true () in
  let (out_read, out_write) =
    try pipe ~cloexec:true ()
    with e -> close in_read; close in_write; raise e in
  let (err_read, err_write) =
    try pipe ~cloexec:true ()
    with e -> close in_read; close in_write;
              close out_read; close out_write; raise e in
  let inchan = in_channel_of_descr in_read in
  let outchan = out_channel_of_descr out_write in
  let errchan = in_channel_of_descr err_read in
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
  (pid, inchan, outchan, errchan)

let open_process_shell fn cmd =
  let shell =
    try Sys.getenv "COMSPEC"
    with Not_found -> raise(Unix_error(ENOEXEC, "open_process_shell", cmd)) in
  fn shell [|shell;"/c ";cmd|]

let open_process cmd =
  open_process_shell open_process_cmdline cmd

let close_process (inchan, outchan, errchan) =
  close_in inchan; close_out outchan; close_in errchan;
