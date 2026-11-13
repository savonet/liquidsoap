(** [input.srt] (spec/input.md), registered when the module loads. Each peer
    gets a reader thread that receives and decodes into the source's buffer; the
    streaming thread takes frames from it and, while the input paces its clock,
    waits for the reader. *)
