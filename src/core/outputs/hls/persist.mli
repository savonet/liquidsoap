(** The state file (spec/persistence.md §2) and the checks a restore runs on it
    as a whole (§4 step 1). Saving and applying are the operator's. A segment's
    [duration] is in seconds; names are relative to the output directory. *)

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
}

type variant = {
  name : string;
  next_position : int;
  next_init_position : int;
  discontinuity_count : int;
  window_start : int;
  pending_tags : string list;
  segments : segment list;
}

val to_string : variant list -> string

(** Parses and checks a state file against the output's variant [names]; [Error]
    carries the reason. *)
val of_string :
  names:string list ->
  valid_name:(string -> bool) ->
  string ->
  (variant list, string) result
