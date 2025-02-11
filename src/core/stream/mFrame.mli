(** MIDI frame manipulation *)

type t = Frame.t

(** Number size of a MIDI frame. *)
val size : unit -> int

(** Get the MIDI content in [Midi] format. Raises [Content.Invalid] if content
    is not [Midi] and [Not_found] if frame has no midi content. *)
val midi : ?field:Frame.field -> t -> Content.Midi.data
