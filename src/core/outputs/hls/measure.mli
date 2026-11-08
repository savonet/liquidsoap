(** Values a variant derives from its listed segments (spec/variants.md §3). *)

(** Peak segment bit rate, bits per second, over runs of consecutive segments
    lasting between 0.5 and 1.5 times [target] seconds, or of the one segment
    while there is a single one. *)
val peak_bitrate : target:int -> Window.segment list -> int option

(** Every format of the listed segments' codec strings, once, in first-seen
    order; [None] if any segment recorded none. *)
val codecs : Window.segment list -> string option
