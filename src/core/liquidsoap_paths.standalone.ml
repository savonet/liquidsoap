type mode = [ `Default | `Standalone | `Posix ]

let mode = `Standalone
let path = Filename.concat (Filename.dirname Sys.executable_name)
let rundir () = path "run"
let rundir_descr = "./run"
let logdir () = path "log"
let logdir_descr = "./log"
let liq_libs_dir () = path "libs"
let liq_libs_dir_descr = "./libs"
let bin_dir () = path "bin"
let bin_dir_descr = "./bin"
let camomile_dir () = path "camomile"
let camomile_dir_descr = "./camomile"

let user_cache_override () =
  let dir = Filename.dirname Sys.executable_name in
  let cwd = Sys.getcwd () in
  Sys.chdir dir;
  let dir = Sys.getcwd () in
  Sys.chdir cwd;
  Some (Filename.concat dir ".cache")

let user_cache_override_descr = "./cache"
let system_cache_override () = Some "./cache"
let system_cache_override_descr = "./cache"
