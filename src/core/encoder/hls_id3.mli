(** The ID3 tag that starts every packed-audio HLS segment
    (spec/encoder-packed-audio.md §7): the metadata's text frames, then a [PRIV]
    frame holding the segment's 33-bit MPEG-2 timestamp. *)

(** [time] is the segment's start in main ticks. *)
val make : version:int -> time:int -> Frame.Metadata.Export.t -> string
