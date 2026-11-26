(** The order in which the packets of one [%ffmpeg] HLS instance reach its
    non-interleaved muxer (encoder.md §8.2). Encoders hand packets over with
    their own latency, video well after audio. A packet is written once every
    gating stream, the streams whose key frames are split points, has produced
    packets up to its time, so that a split at a key frame has every stream's
    earlier packets before it and none of the later ones. Each stream keeps its
    own order. A packet held longer than the keyframe wait behind the latest one
    is written anyway. *)

type t

val create : unit -> t

(** [time] is the media time of the packet; [None] places it right after the
    stream's previous one. *)
val push : t -> field:string -> time:int option -> write:(unit -> unit) -> unit

(** Writes every packet that is ready, earliest first; [all] writes every held
    packet, at the end of a container. *)
val release : ?all:bool -> t -> gating:string list -> unit
