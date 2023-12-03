(** MIDI frame manipulation *)

type t = Frame.t

(** Is it partially filled ? *)
val is_partial : t -> bool

(** Number size of a MIDI frame. *)
val size : unit -> int

(** Position of the first break. *)
val position : t -> int

(** Add a track mark. *)
val add_track_mark : t -> int -> t

type metadata = Frame.metadata

val add_metadata : t -> int -> metadata -> t
val get_metadata : t -> int -> metadata option
val get_all_metadata : t -> (int * metadata) list

(** Get the MIDI content. Raises [Not_found] if frame has
    no midi content. *)
val content : ?field:Frame.field -> t -> Content.data

(** Get the MIDI content in [Midi] format. Raises [Content.Invalid]
  * if content is not [Midi] and [Not_found] if frame has no midi content. *)
val midi : ?field:Frame.field -> t -> Content.Midi.data
