(** A wait that blocks in a library until the stream for a time is due. [until]
    returns [`Interrupted] once [interrupted ()] holds; [interrupt] makes every
    wait in progress check it again. *)
type blocking_wait = {
  until :
    interrupted:(unit -> bool) -> float -> [ `Due | `Ended | `Interrupted ];
  interrupt : unit -> unit;
}

(** [`Timer delay]: [delay target] is the real time after which [now] will have
    reached [target]. *)
type wait = [ `Timer of float -> float | `Blocking of blocking_wait ]

(** [now] never goes backwards and advances. *)
type time_source = { label : string; now : unit -> float; wait : wait }

(** A field is given only where it differs from the clock's default. *)
type timed = {
  time_source : time_source option;
  latency : float option;
  max_latency : float option;
}

type pacing = [ `Self_paced | `Timed of timed ]

(** What sets the pace of a stream by its own means: spec/pacing.md §2. *)
type t = private { identity : int; name : string; pacing : pacing }

(** One call per pacing entity: the result is equal to itself only. *)
val make : name:string -> pacing -> t

(** A source declared self-sync: [`Timed] with every default. *)
val generic : name:string -> t

val equal : t -> t -> bool
val same : t option -> t option -> bool
val blocks : t -> bool
val string_of_pacing : pacing -> string

(** Monotonic, with a [`Timer] wait. Registered as ["ocaml"]. *)
val builtin : time_source

(** Time sources by the name [clock.preferred] selects. *)
val time_sources : (string, time_source) Hashtbl.t

(** Looks into [time_sources], then into [Liq_time.implementations]. *)
val find_time_source : string -> time_source option
