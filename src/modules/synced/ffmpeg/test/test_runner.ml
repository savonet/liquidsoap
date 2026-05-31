let is_github_actions = Sys.getenv_opt "GITHUB_ACTIONS" <> None

let start_group name =
  if is_github_actions then Printf.printf "::group::%s\n%!" name
  else
    Printf.printf
      "\n\
       ========================================\n\
      \  %s\n\
       ========================================\n\
       %!"
      name

let end_group () =
  if is_github_actions then print_endline "::endgroup::"
  else print_endline "----------------------------------------"

let run_command args =
  let args = Array.of_list args in
  let pid =
    Unix.create_process
      ("./" ^ args.(0))
      args Unix.stdin Unix.stdout Unix.stderr
  in
  match snd (Unix.waitpid [] pid) with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED n -> 128 + n
    | Unix.WSTOPPED _ -> 128

let () =
  let args = Array.to_list Sys.argv in
  match args with
    | _ :: name :: cmd :: rest ->
        start_group name;
        let exit_code = run_command (cmd :: rest) in
        if exit_code = 0 then Printf.printf "PASSED: %s\n%!" name
        else (
          Printf.printf "FAILED: %s (exit code %d)\n%!" name exit_code;
          if is_github_actions then
            Printf.printf "::error::Test %s failed\n%!" name);
        end_group ();
        exit exit_code
    | _ ->
        prerr_endline "Usage: test_runner <test_name> <command> [args...]";
        exit 1
