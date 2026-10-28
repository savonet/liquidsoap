(** ICY in-stream metadata: negotiation, block format and stamping. *)

val empty_block : string

(** Whether a MIME type, up to its first [;], is an Ogg one. *)
val is_ogg_mime : string -> bool

(** Whether a request with these headers (lowercased names) gets ICY metadata on
    an output of this MIME type. *)
val negotiated : mime:string -> (string * string) list -> bool

(** One metadata event of the output, with its block built once. *)
type event = { generation : int; block : string }

(** The block for a title and a url, truncated to fit one length byte. *)
val block : url:string option -> string option -> string

(** [artist - title], [artist] or [title], from the metadata. *)
val title : (string * string) list -> string option

val event :
  generation:int -> url:string option -> (string * string) list -> event

(** The block preceding a stream byte whose metadata in effect is [stamp], for a
    listener that last received [last_generation]: the event's block when its
    generation differs, the empty block otherwise. Returns the block and the
    listener's new last generation. *)
val block_for : last_generation:int -> event option -> string * int

(** A listener's place in the ICY framing: stream bytes since the last block,
    and the generation of the last non-empty block it received. *)
type position = { since_block : int; last_generation : int }

val start : position

(** [interleave ~metaint position stamp data] inserts blocks into [data], every
    block stamped with [stamp]. A block is emitted only before a stream byte,
    never at the end of [data]. *)
val interleave :
  metaint:int -> position -> event option -> string -> string * position
