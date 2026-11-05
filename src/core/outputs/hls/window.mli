(** One variant's published segments: position numbers, the discontinuity
    counter, the sliding window (spec/playlists.md §2, §2.1) and retention
    (spec/files.md §5). Pure: deleting the files of {!expire}'s result is the
    caller's.

    After each publish the caller runs {!slide}, writes the media playlist of
    {!listed}, then calls {!mark_listed} and {!expire}. A segment's [duration]
    is in seconds and [size] in bytes; [listed_max] and [left_at] feed the
    availability allowance. *)

type segment = {
  position : int;
  discontinuous : bool;
  discontinuity : int;
  forced : bool;
  codecs : string option;
  init : string option;
  name : string;
  tags : string list;
  duration : float;
  size : int;
  mutable listed_max : float;
  mutable left_at : float option;
}

(** [segments] are the retained ones, oldest first; [closed] is the media time,
    in seconds, of the segments closed in this process. *)
type t = {
  mutable next_position : int;
  mutable next_init_position : int;
  mutable discontinuity_count : int;
  mutable segments : segment list;
  mutable window_start : int;
  mutable closed : float;
}

val create : unit -> t

val restore :
  next_position:int ->
  next_init_position:int ->
  discontinuity_count:int ->
  window_start:int ->
  (Persist.segment * int) list ->
  t

val to_persisted : t -> Persist.segment list
val listed : t -> segment list

(** Total duration, in seconds. *)
val duration : segment list -> float

val media_sequence : t -> int
val discontinuity_sequence : t -> int

(** Takes the next init position. *)
val next_init_position : t -> int

val publish :
  t ->
  discontinuous:bool ->
  forced:bool ->
  codecs:string option ->
  init:string option ->
  name:string ->
  tags:string list ->
  duration:float ->
  size:int ->
  segment

val slide : t -> max_segments:int -> target:int -> unit
val mark_listed : t -> unit

(** Removes and returns the segments past both retention allowances; none when
    [overhead] is [None]. *)
val expire : t -> overhead:int option -> segment list

val referenced_inits : t -> string list
