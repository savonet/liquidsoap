let log = Log.make ["clock"]
let scheduler = Scheduler.raw
let conf = Dtools.Conf.void ~p:(Configure.conf#plug "clock") "Clock settings"

let conf_latency =
  Dtools.Conf.float ~p:(conf#plug "latency") ~d:0.1
    "How much stream a clock produces ahead of its time source before it \
     rests, in seconds."

let conf_max_latency =
  Dtools.Conf.float ~p:(conf#plug "max_latency") ~d:60.
    "Lateness beyond which a clock resets its stream instead of catching up, \
     in seconds."

let conf_log_delay =
  Dtools.Conf.float ~p:(conf#plug "log_delay") ~d:1.
    "Minimum time between two latency warnings of one clock, in seconds."

let conf_log_delay_threshold =
  Dtools.Conf.float
    ~p:(conf#plug "log_delay_threshold")
    ~d:0.2 "Lateness below which no latency warning is logged, in seconds."

let conf_preferred =
  Dtools.Conf.string ~p:(conf#plug "preferred") ~d:"posix"
    "Preferred time source. One of: \"posix\" or \"ocaml\"."

let conf_task =
  Dtools.Conf.bool ~p:(conf#plug "task") ~d:true
    "Run clocks as scheduler tasks. When off, every clock has its own thread."

let conf_leak_warning =
  Dtools.Conf.int ~p:(conf#plug "leak_warning") ~d:50
    "Number of activated sources at each multiple of which a leak warning is \
     logged."

let conf_time_box =
  Dtools.Conf.float ~p:(conf#plug "time_box") ~d:0.1
    "Longest a clock keeps a scheduler worker while it does not rest, in \
     seconds."

let conf_shutdown_wait =
  Dtools.Conf.float
    ~p:(conf#plug "shutdown_wait")
    ~d:10.
    "Longest the application waits at shutdown for clocks to stop, in seconds."

let conf_thread_lease =
  Dtools.Conf.float ~p:(conf#plug "thread_lease") ~d:5.
    "A blocking source that leaves a clock and returns within this many \
     seconds is flapping. Its clock then keeps its thread for this long after \
     each drop, until the source stays away longer. With 0, a clock always \
     gives its thread back at once."

let rec push queue value =
  let values = Atomic.get queue in
  if not (Atomic.compare_and_set queue values (value :: values)) then
    push queue value
