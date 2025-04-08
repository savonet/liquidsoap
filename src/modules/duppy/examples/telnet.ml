type priority = Non_blocking | Maybe_blocking

let io_priority = Non_blocking

(* Create scheduler *)
let scheduler = Duppy.create ()

(* Create two queues,
 * one for non blocking events
 * and another for blocking
 * events *)
let new_queue ~priority ~name () =
  let log = Printf.printf "%s: %s\n%!" name in
  let priorities p = p = priority in
  let queue () = Duppy.queue scheduler ~log ~priorities name in
  Thread.create queue ()

let th =
  ignore (new_queue ~priority:Non_blocking ~name:"Non blocking queue" ());
  ignore (new_queue ~priority:Maybe_blocking ~name:"Maybe blocking queue #1" ());
  new_queue ~priority:Maybe_blocking ~name:"Maybe blocking queue #2" ()

let exec_command s () =
  let chan = Unix.open_process_in s in
  let rec aux () =
    match try Some (input_line chan) with End_of_file -> None with
      | None -> []
      | Some s -> s :: aux ()
  in
  let l = aux () in
  ignore (Unix.close_process_in chan);
  Duppy.Monad.return (String.concat "\r\n" l)

let commands = Hashtbl.create 10

let () =
  Hashtbl.add commands "hello" (false, fun () -> Duppy.Monad.return "world");
  Hashtbl.add commands "foo" (false, fun () -> Duppy.Monad.return "bar");
  Hashtbl.add commands "uptime" (true, exec_command "uptime");
  Hashtbl.add commands "date" (true, exec_command "date");
  Hashtbl.add commands "whoami" (true, exec_command "whoami");
  Hashtbl.add commands "sleep" (true, exec_command "sleep 15");
  Hashtbl.add commands "exit" (true, fun () -> Duppy.Monad.raise ())

(* Add commands here *)
let help = Buffer.create 10

let () =
  Buffer.add_string help "List of commands:";
  Hashtbl.iter
    (fun x _ -> Buffer.add_string help (Printf.sprintf "\r\n%s" x))
    commands;
  Hashtbl.add commands "help"
    (false, fun () -> Duppy.Monad.return (Buffer.contents help))

let handle_client socket =
  let on_error e =
    match e with
      | Duppy.Io.Io_error -> Printf.printf "Client disconnected"
      | Duppy.Io.Unix (c, p, m) ->
          Printf.printf "%s" (Printexc.to_string (Unix.Unix_error (c, p, m)))
      | Duppy.Io.Unknown e -> Printf.printf "%s" (Printexc.to_string e)
      | Duppy.Io.Timeout -> Printf.printf "Timeout"
  in
  let h = { Duppy.Monad.Io.scheduler; socket; data = ""; on_error } in
  (* Read and process lines *)
  let rec exec () =
    let __pa_duppy_0 =
      Duppy.Monad.Io.read ?timeout:None ~priority:io_priority
        ~marker:(Duppy.Io.Split "[\r\n]+") h
    in
    Duppy.Monad.bind __pa_duppy_0 (fun req ->
        let __pa_duppy_0 =
          try
            let blocking, command = Hashtbl.find commands req in
            if not blocking then command ()
            else Duppy.Monad.Io.exec ~priority:Maybe_blocking h (command ())
          with Not_found ->
            Duppy.Monad.return
              "ERROR: unknown command, type \"help\" to get a list of commands."
        in
        Duppy.Monad.bind __pa_duppy_0 (fun ans ->
            Duppy.Monad.bind
              (Duppy.Monad.bind
                 (Duppy.Monad.Io.write ?timeout:None ~priority:io_priority h
                    (Bytes.unsafe_of_string "BEGIN\r\n"))
                 (fun () ->
                   Duppy.Monad.bind
                     (Duppy.Monad.Io.write ?timeout:None ~priority:io_priority h
                        (Bytes.unsafe_of_string ans))
                     (fun () ->
                       Duppy.Monad.Io.write ?timeout:None ~priority:io_priority
                         h
                         (Bytes.unsafe_of_string "\r\nEND\r\n"))))
              (fun () -> exec ())))
  in
  let close () = try Unix.close socket with _ -> () in
  let return () =
    let on_error e =
      on_error e;
      close ()
    in
    Duppy.Io.write ~priority:io_priority ~on_error ~exec:close scheduler
      ~string:(Bytes.unsafe_of_string "Bye!\r\n")
      socket
  in
  Duppy.Monad.run ~return ~raise:close (exec ())

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
  Thread.join th
