(* Availability detection, spec/build.md §2.

   Sub-commands, all run by dune rules on the build machine:
   - [detect]: decides whether one binding library is available in one
     context and writes its availability, flags and versions files;
   - [check]: the install-time check, fails when the library is unavailable;
   - [all]: prints whether every listed library is available;
   - [require]: fails naming the unavailable libraries, for the test alias;
   - [stanzas]: prints a file of dune stanzas when every listed library is
     available and nothing otherwise, for stanzas dune cannot gate itself.

   Everything about the target comes from the arguments and the environment. *)

let fail fmt =
  Printf.ksprintf
    (fun msg ->
      prerr_endline msg;
      exit 1)
    fmt

let read_file path = In_channel.with_open_bin path In_channel.input_all

let write_file path content =
  Out_channel.with_open_bin path (fun oc -> output_string oc content)

let words s = String.split_on_char ' ' s |> List.filter (fun w -> w <> "")
let is_available name = String.trim (read_file (name ^ "_available")) = "true"

(* pkg-config as selected for a context, spec/build.md §2.2. *)
module Pkg_config = struct
  type t = { command : string list; env : string array }

  let select ~context =
    let context =
      Option.value (Sys.getenv_opt "LIQUIDSOAP_DUNE_TARGET") ~default:context
    in
    let ctx = String.map (function '.' -> '_' | c -> c) context in
    let command =
      match Sys.getenv_opt ("PKG_CONFIG_" ^ ctx) with
        | Some command -> command
        | None ->
            Option.value (Sys.getenv_opt "PKG_CONFIG") ~default:"pkg-config"
    in
    let env =
      match Sys.getenv_opt ("PKG_CONFIG_PATH_" ^ ctx) with
        | None -> Unix.environment ()
        | Some path ->
            let is_path v = String.starts_with ~prefix:"PKG_CONFIG_PATH=" v in
            Unix.environment () |> Array.to_list
            |> List.filter (fun v -> not (is_path v))
            |> List.cons ("PKG_CONFIG_PATH=" ^ path)
            |> Array.of_list
    in
    { command = words command; env }

  (* [None] when pkg-config cannot be run or exits with a failure. *)
  let run t args =
    match t.command with
      | [] -> None
      | program :: _ -> (
          let argv = Array.of_list (t.command @ args) in
          let output, input = Unix.pipe ~cloexec:true () in
          match
            Unix.create_process_env program argv t.env Unix.stdin input
              Unix.stderr
          with
            | exception Unix.Unix_error _ ->
                Unix.close output;
                Unix.close input;
                None
            | pid -> (
                Unix.close input;
                let ic = Unix.in_channel_of_descr output in
                let stdout = In_channel.input_all ic in
                In_channel.close ic;
                match Unix.waitpid [] pid with
                  | _, Unix.WEXITED 0 -> Some (String.trim stdout)
                  | _ -> None))

  (* ponytail: flags are split on blanks, a quoted path containing a space
     is cut; parse pkg-config's shell quoting if one ever shows up. *)
  let flags t query modules =
    Option.map
      (fun out -> words (String.map (function '\n' -> ' ' | c -> c) out))
      (run t (query :: modules))
end

(* spec/build.md §2.4. *)
let filter_windows_link_flags =
  List.filter (fun flag ->
      not
        ((String.length flag >= 3 && String.starts_with ~prefix:"-Wl" flag)
        || List.mem flag ["-static-libgcc"; "-lssp"; "-lmingw32"]))

(* Warnings are errors, and the tables are checked against the compiler's
   view of each enumeration, everywhere but in a packaged build. *)
let profile_c_flags profile =
  let checked = ["-Werror"; "-DOCAML_FFMPEG_CHECK_TABLES"] in
  match profile with
    | "release" -> []
    | "asan" ->
        checked @ ["-g"; "-fno-omit-frame-pointer"; "-fsanitize=address"]
    | "gcstress" -> checked @ ["-g"; "-DOCAML_FFMPEG_GC_STRESS"]
    | _ -> checked

let profile_link_flags = function "asan" -> ["-fsanitize=address"] | _ -> []

let sexp flags =
  "(" ^ String.concat " " (List.map (Printf.sprintf "%S") flags) ^ ")\n"

type detected = {
  c_flags : string list;
  link_flags : string list;
  versions : string;
}

let excluded name =
  List.mem name
    (words
       (Option.value
          (Sys.getenv_opt "LIQUIDSOAP_MINIMAL_EXCLUDE_DEPS")
          ~default:""))

(* [requirement] is a comma-separated list of "module >= version". *)
let detect ~context ~requires ~requirement name =
  let ( let* ) = Option.bind in
  let* () = if excluded name then None else Some () in
  let* () = if List.for_all is_available requires then Some () else None in
  let pkg_config = Pkg_config.select ~context in
  let requirements =
    String.split_on_char ',' requirement |> List.map String.trim
  in
  let modules = List.map (fun r -> List.hd (words r)) requirements in
  let* _ =
    Pkg_config.run pkg_config ("--print-errors" :: "--exists" :: requirements)
  in
  let* c_flags = Pkg_config.flags pkg_config "--cflags" modules in
  let* link_flags = Pkg_config.flags pkg_config "--libs" modules in
  let* versions = Pkg_config.run pkg_config ("--modversion" :: modules) in
  Some { c_flags; link_flags; versions }

let detect_command args =
  let os_type = ref "" and context = ref "" and profile = ref "release" in
  let requires = ref [] and positional = ref [] in
  let rec parse = function
    | "--os-type" :: v :: rest ->
        os_type := v;
        parse rest
    | "--context" :: v :: rest ->
        context := v;
        parse rest
    | "--profile" :: v :: rest ->
        profile := v;
        parse rest
    | "--requires" :: v :: rest ->
        requires := words v;
        parse rest
    | arg :: rest ->
        positional := arg :: !positional;
        parse rest
    | [] -> ()
  in
  parse args;
  let name, requirement, extra_c_flags =
    match List.rev !positional with
      | name :: requirement :: extra -> (name, requirement, extra)
      | _ -> fail "usage: detect [options] <library> <requirement> [c flags]"
  in
  let detected =
    detect ~context:!context ~requires:!requires ~requirement name
  in
  let available, c_flags, link_flags, versions =
    match detected with
      | None -> (false, [], [], "")
      | Some { c_flags; link_flags; versions } ->
          let link_flags =
            if !os_type = "Win32" then filter_windows_link_flags link_flags
            else link_flags
          in
          ( true,
            c_flags @ extra_c_flags @ profile_c_flags !profile,
            link_flags @ profile_link_flags !profile,
            versions )
  in
  write_file (name ^ "_available") (string_of_bool available);
  write_file (name ^ "_c_flags")
    (String.concat "" (List.map (fun f -> f ^ "\n") c_flags));
  write_file (name ^ "_c_flags.sexp") (sexp c_flags);
  write_file (name ^ "_c_library_flags.sexp") (sexp link_flags);
  write_file (name ^ "_versions") (versions ^ "\n")

let unavailable names = List.filter (fun name -> not (is_available name)) names

let () =
  match List.tl (Array.to_list Sys.argv) with
    | "detect" :: args -> detect_command args
    | ["check"; name] ->
        if String.trim (In_channel.input_all stdin) <> "true" then
          fail
            "The ffmpeg library %s is not available: FFmpeg 7.1 or later and \
             pkg-config are required."
            name
    | "all" :: names -> print_string (string_of_bool (unavailable names = []))
    | "stanzas" :: file :: names ->
        if unavailable names = [] then print_string (read_file file)
    | "require" :: names -> (
        match unavailable names with
          | [] -> ()
          | missing ->
              fail "The ffmpeg tests need these unavailable libraries: %s"
                (String.concat ", " missing))
    | _ -> fail "usage: detect (detect|check|all|require|stanzas) ..."
