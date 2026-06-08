module S = Lo.Server

let handler path data =
  let s = function
    | `Int32 n -> string_of_int n
    | `Float f | `Double f -> Printf.sprintf "%f" f
    | `String s -> s
    | `True -> "True"
    | `False -> "False"
    | _ -> "???"
  in
  let data = Array.to_list data in
  let s = List.map s data in
  let s = String.concat ", " s in
  Printf.printf "Message on %s: %s\n%!" path s

let () =
  let port =
    if Array.length Sys.argv < 2 then 7777 else int_of_string Sys.argv.(1)
  in
  let s = S.create port handler in
  while true do
    Gc.full_major ();
    S.recv s
  done
