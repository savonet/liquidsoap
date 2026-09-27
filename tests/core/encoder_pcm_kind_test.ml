open Liquidsoap_lang

(* Only %ffmpeg encodes pcm_s16 or pcm_f32 audio. *)

let typecheck encoder =
  Typechecking.check
    ~throw:(fun ~bt exn -> Printexc.raise_with_backtrace exn bt)
    ~check_top_level_override:false
    (Term.make (`Encoder encoder))

let () = typecheck ("mp3", [`Anonymous "mono"])

let rejected encoder =
  match typecheck encoder with
    | () -> false
    | exception Runtime_error.Runtime_error { kind = "encoder"; _ } -> true

let () =
  List.iter
    (fun encoder -> assert (rejected encoder))
    [
      ("mp3", [`Anonymous "mono"; `Anonymous "pcm_s16"]);
      ("vorbis", [`Anonymous "pcm_f32"]);
      ("ogg", [`Encoder ("vorbis", [`Anonymous "pcm_s16"])]);
    ]
