val log : Log.t
val scheduler : Tutils.priority Duppy.scheduler
val conf : Dtools.Conf.ut
val conf_latency : float Dtools.Conf.t
val conf_max_latency : float Dtools.Conf.t
val conf_log_delay : float Dtools.Conf.t
val conf_log_delay_threshold : float Dtools.Conf.t
val conf_preferred : string Dtools.Conf.t
val conf_task : bool Dtools.Conf.t
val conf_leak_warning : int Dtools.Conf.t
val conf_time_box : float Dtools.Conf.t
val conf_shutdown_wait : float Dtools.Conf.t
val conf_thread_lease : float Dtools.Conf.t

(** Lock-free push on a list held in an atomic. *)
val push : 'a list Atomic.t -> 'a -> unit
