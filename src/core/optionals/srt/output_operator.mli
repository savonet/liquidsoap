(** [output.srt] (spec/output.md), registered when the module loads. The
    streaming thread encodes and cuts each peer's bytes into payloads on that
    peer's queue; one sender thread per peer hands them to libsrt, so the
    streaming thread never waits for a peer. *)
