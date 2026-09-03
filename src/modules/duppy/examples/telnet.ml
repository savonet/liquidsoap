type priority = Non_blocking | Maybe_blocking

let io_priority = Non_blocking

(* Reading and writing to the socket never blocks, so it runs directly on a
   domain of the pool. Running a command does block, so it gets a thread. *)
let scheduler =
  Duppy.create
    ~classify:(function
      | Non_blocking -> `Immediate | Maybe_blocking -> `Blocking)
    ()

let exec_command s () =
  let chan = Unix.open_process_in s in
  let rec aux () =
    match try Some (input_line chan) with End_of_file -> None with
      | None -> []
      | Some s -> s :: aux ()
  in
  let l = aux () in
  ignore (Unix.close_process_in chan);
  String.concat "\r\n" l

exception Quit

let commands = Hashtbl.create 10

let () =
  Hashtbl.add commands "hello" (false, fun () -> "world");
  Hashtbl.add commands "foo" (false, fun () -> "bar");
  Hashtbl.add commands "uptime" (true, exec_command "uptime");
  Hashtbl.add commands "date" (true, exec_command "date");
  Hashtbl.add commands "whoami" (true, exec_command "whoami");
  Hashtbl.add commands "sleep" (true, exec_command "sleep 15");
  Hashtbl.add commands "exit" (true, fun () -> raise Quit)

(* Add commands here *)
let help = Buffer.create 10

let () =
  Buffer.add_string help "List of commands:";
  Hashtbl.iter
    (fun x _ -> Buffer.add_string help (Printf.sprintf "\r\n%s" x))
    commands;
  Hashtbl.add commands "help" (false, fun () -> Buffer.contents help)

let handle_client socket =
  let report = function
    | Duppy.Io.Io_error -> Printf.printf "Client disconnected"
    | Duppy.Io.Unix (c, p, m, _) ->
        Printf.printf "%s" (Printexc.to_string (Unix.Unix_error (c, p, m)))
    | Duppy.Io.Unknown (e, _) -> Printf.printf "%s" (Printexc.to_string e)
    | Duppy.Io.Timeout -> Printf.printf "Timeout"
  in
  let h = Duppy.Io.handle scheduler socket in
  let write s =
    Duppy.Io.write ~priority:io_priority h (Bytes.unsafe_of_string s)
  in
  let close () = try Unix.close socket with _ -> () in
  let rec exec () =
    let req =
      Duppy.Io.read ~priority:io_priority h (Duppy.Io.Split "[\r\n]+")
    in
    let ans =
      match Hashtbl.find_opt commands req with
        | Some (blocking, command) ->
            if blocking then Duppy.reschedule ~priority:Maybe_blocking scheduler;
            command ()
        | None ->
            "ERROR: unknown command, type \"help\" to get a list of commands."
    in
    write "BEGIN\r\n";
    write ans;
    write "\r\nEND\r\n";
    exec ()
  in
  Duppy.run (fun () ->
      (try exec () with
        | Quit -> ( try write "Bye!\r\n" with Duppy.Io.Error e -> report e)
        | Duppy.Io.Error e -> report e);
      close ())

open Unix

let port = 4123
let bind_addr_inet = inet_addr_of_string "0.0.0.0"
let bind_addr = ADDR_INET (bind_addr_inet, port)
let max_conn = 10
let sock = socket PF_INET SOCK_STREAM 0

let () =
  setsockopt sock SO_REUSEADDR true;
  let rec incoming _ =
    (try
       let s, caller = accept sock in
       let ip =
         let a =
           match caller with ADDR_INET (a, _) -> a | _ -> assert false
         in
         try (gethostbyaddr a).h_name with Not_found -> string_of_inet_addr a
       in
       Printf.printf "New client: %s\n" ip;
       handle_client s
     with e ->
       Printf.printf "Failed to accept new client: %S\n" (Printexc.to_string e));
    [
      {
        Duppy.Task.priority = io_priority;
        Duppy.Task.events = [`Read sock];
        Duppy.Task.handler = incoming;
      };
    ]
  in
  (try bind sock bind_addr
   with Unix.Unix_error (Unix.EADDRINUSE, "bind", "") ->
     failwith (Printf.sprintf "port %d already taken" port));
  listen sock max_conn;
  Duppy.Task.add scheduler
    {
      Duppy.Task.priority = io_priority;
      Duppy.Task.events = [`Read sock];
      Duppy.Task.handler = incoming;
    };
  Duppy.start ~log:(Printf.printf "telnet: %s\n%!") scheduler;
  (* The pool runs on its own domains, so this thread has nothing left to do. *)
  while true do
    Unix.sleep 3600
  done
