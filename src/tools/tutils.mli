
(** Multithreading utilities *)

(** Make a function work in critical section, protected by a given lock. *)
val mutexify : Mutex.t -> ('a -> 'b) -> ('a -> 'b)

(** Thread wrapper.
  Give names to threads, and forbid them to raise an exception.
  The main process is expected to run raise_everything after having launched
  the needed threads. So the main process will sleep, until a thread
  aborts. In that case it will log and output the error, and return. *)
val create : ('a -> unit) -> 'a -> string -> Thread.t
val raise_everything : unit -> unit

(** Wait for threads to terminate, return after 3 seconds even if
  * the threads didn't return. *)
val join_all : unit -> unit

(** Wait for the threads to terminate,
  * never return if some thread keeps running. *)
val join_all_no_timeout : unit -> unit

(** Check if a thread is running. *)
val running : string -> Thread.t -> bool

(** [Mutex] provides monitoring of locked threads, for debugging. *)
module Mutex : sig
  val create : string -> Mutex.t

  (** Get the list of Tutiles mutexes, with name and locking information. *)
  val get_list : unit -> (string*bool*Mutex.t) list
end

(** Get the list of Tutils named threads. *)
val get_list : unit -> (string*Thread.t) list
