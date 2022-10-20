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
