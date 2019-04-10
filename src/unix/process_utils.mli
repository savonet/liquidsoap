type t = {
  pid:    int;
  stdin:  out_channel;
  stdout: in_channel;
  stderr: in_channel
}

val wait          : t -> int * Unix.process_status
val open_process  : string -> string array -> t
val close_process : t -> unit
