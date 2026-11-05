(** The files of one HLS output (spec/files.md §1–§3, §5.1, §8): names checked
    against the directory, atomic writes, the tracked set and the file-change
    events.

    {!write} publishes a file and returns its event, which the caller fires once
    the state that names the file is saved. {!delete} fires [deleted], then
    unlinks. The tracked set is read from other threads through {!tracked}. *)

type kind = [ `Playlist | `Segment of string | `Init of string ]
type t

(** [on_event ~state ~path] is called for every event, in the streaming cycle;
    [reserved] names are refused by {!valid_name}. *)
val create :
  dir:string ->
  temp_dir:string option ->
  perm:int ->
  dir_perm:int ->
  reserved:string list ->
  on_event:(state:string -> path:string -> unit) ->
  unit ->
  t

val dir : t -> string

(** Relative, without [..], neither reserved nor tracked. *)
val valid_name : t -> string -> bool

(** Temporary files of the output carry this prefix. *)
val temp_prefix : string

(** Writes and publishes [name], tracked as [kind]; the result fires its event.
    On failure the temporary file is removed and nothing is tracked. *)
val write : t -> name:string -> kind -> Strings.t -> unit -> unit

(** [write] followed by its event. *)
val write_now : t -> name:string -> kind -> Strings.t -> unit

(** A missing file or a failed unlink is a warning. *)
val delete : t -> string -> unit

(** Tracks a file found on disk, for a restore; no event. *)
val track : t -> string -> kind -> unit

val is_tracked : t -> string -> bool

(** A snapshot. *)
val tracked : t -> (string * kind) list
