include Liquidsoap_lang.Multicore

let conf_multicore =
  Dtools.Conf.bool
    ~p:(Configure.conf#plug "multicore")
    ~d:true "Multicore configuration"

let conf_multicore_domains_count =
  Dtools.Conf.int
    ~p:(conf_multicore#plug "concurrency")
    ~d:(Atomic.get domains_count)
    "Multicore concurrency. It is recommended to leave the default here."

let () =
  conf_multicore#on_change (Atomic.set multicore);
  conf_multicore_domains_count#on_change (Atomic.set domains_count)
