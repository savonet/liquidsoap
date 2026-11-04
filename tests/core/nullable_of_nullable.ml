(* A nullable over a variable later bound to a nullable is [t??], which is
   [t?]: it prints as [string?] and is a subtype of [string?]. *)

open Type

let () = Frame_settings.lazy_config_eval := true

let () =
  let nullable_string () = make (Nullable (make String)) in
  let variable = var () in
  let nullable_of_variable = make (Nullable variable) in
  Typing.(nullable_string () <: variable);
  let shown = to_string nullable_of_variable in
  if shown <> "string?" then
    failwith (Printf.sprintf "printed as %s, expected string?" shown);
  Typing.(nullable_of_variable <: nullable_string ());
  print_endline "nullable_of_nullable: 2 checks passed"
