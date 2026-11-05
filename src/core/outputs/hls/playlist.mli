(** Media playlist bytes and URIs (spec/playlists.md §1, §2, §4). An [entry] is
    one listed segment, its [duration] in seconds and its [tags] those inserted
    for it. *)

type entry = {
  uri : string;
  duration : float;
  discontinuous : bool;
  init_uri : string option;
  tags : string list;
}

type media = {
  target : int;
  independent : bool;
  media_sequence : int;
  discontinuity_sequence : int;
  extra_tags : string list;
  entries : entry list;
}

(** The [EXT-X-VERSION] of every media playlist, and the version given to the
    main playlist writer. *)
val version : int

(** [uri ~prefix name]: [name] relative to the output directory, each path
    segment percent-encoded, after [prefix]. *)
val uri : prefix:string -> string -> string

val media : media -> string
