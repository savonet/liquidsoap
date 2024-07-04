type mode = [ `Default | `Standalone | `Posix ]

let mode = `Posix
let rundir () = "/var/run/liquidsoap"
let rundir_descr = rundir ()
let logdir () = "/var/log/liquidsoap"
let logdir_descr = logdir ()
let liq_libs_dir () = "/usr/share/liquidsoap/libs"
let liq_libs_dir_descr = liq_libs_dir ()
let bin_dir () = "/usr/share/liquidsoap/bin"
let bin_dir_descr = bin_dir ()
let camomile_dir () = "/usr/share/liquidsoap/camomile"
let camomile_dir_descr = camomile_dir ()
let user_cache_override () = None
let user_cache_override_descr = "$HOME/.cache/liquidsoap"
let system_cache_override () = Some "/var/cache/liquidsoap"
let system_cache_override_descr = "/var/cache/liquidsoap"
