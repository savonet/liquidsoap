(** Where a variant's byte stream is cut into segments: the cadence of key-frame
    variants, the grid of free-split ones, sync cuts and container restarts
    (spec/segmenting.md). Pure: no encoder, no file.

    One value per variant and per run. For each chunk, the caller runs
    {!ensure_open}, encodes with {!request} as the boundary, calls {!advance}
    with the chunk's length, then applies the encoder's pieces in order with
    {!add_bytes}, {!split} and {!restart}. {!sync} requests an output-wide cut.
    {!finish} closes the open segment at a stop; {!abort} drops it.

    Segments are opened through [on_open] and handed back through [on_close],
    publishable or not. *)

(** [duration] is [D], in ticks, a positive multiple of a frame. *)
type config = { duration : int; ticks_per_second : int }

(** [ticks] in seconds, rounded to the nearest integer, halves up. *)
val rounded_seconds : config -> int -> int

(** [T], in seconds: [D] rounded to the nearest second, halves up, at least 1.
*)
val target_duration : config -> int

(** A segment handed back by the segmenter:
    - [length]: in ticks;
    - [discontinuous]: opened by a restart or a discontinuous sync cut;
    - [cut]: what ended it;
    - [publishable]: false when it holds no encoder byte or has length 0, and
      must be dropped. *)
type 'a closed = {
  payload : 'a;
  start : int;
  length : int;
  bytes : Strings.t;
  discontinuous : bool;
  cut : cut;
  publishable : bool;
}

and cut = [ `Cadence | `Grid | `Sync | `Restart | `End ]

(** How a variant's split points are turned into cuts: [`Grid] for free-split
    variants, [`Key_frames] for the others (spec/segmenting.md §4). *)
type cadence = [ `Grid | `Key_frames ]

type 'a t

(** [on_open ~start] returns the new segment's payload and its prefix bytes (ID3
    tag, then encoder header), which precede the encoder bytes. *)
val create :
  on_open:(start:int -> 'a * Strings.t) ->
  on_close:('a closed -> unit) ->
  config ->
  'a t

(** Media time of the run, in ticks. *)
val media_time : 'a t -> int

(** The open segment's payload. *)
val current : 'a t -> 'a option

(** The next media time at which a split point is wanted: the first grid
    boundary at or after the media time, or an earlier pending sync. *)
val request : 'a t -> int

(** Opens a segment at the media time when none is open. *)
val ensure_open : 'a t -> unit

val advance : 'a t -> int -> unit
val add_bytes : 'a t -> Strings.t -> unit
val split : 'a t -> cadence:cadence -> time:int -> unit

(** A container restart at [time]: cut at the current byte position, the new
    segment is discontinuous. *)
val restart : 'a t -> time:int -> unit

val sync : 'a t -> time:int -> discontinuous:bool -> unit
val finish : 'a t -> unit
val abort : 'a t -> 'a option
