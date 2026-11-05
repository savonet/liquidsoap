(** The HLS seam over a plain encoder of raw MPEG audio or ADTS
    (spec/encoder-packed-audio.md): every chunk start is a split point and every
    segment starts with an ID3 tag carrying its timestamp. Registered for
    [%mp3], [%shine] and [%fdkaac] with ADTS transport. *)
