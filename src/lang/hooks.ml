let type_of_encoder =
  ref (fun ~pos:_ _ -> failwith "Encoders are not implemented!")

type encoder_params =
  (string * [ `Value of Value.t | `Encoder of encoder ]) list

and encoder = string * encoder_params

let make_encoder =
  ref (fun ~pos:_ _ _ -> failwith "Encoders are not implemented!")

let has_encoder = ref (fun _ -> false)
let liq_libs_dir = ref (fun () -> raise Not_found)
let version = ref (fun () -> raise Not_found)
let log_path = ref None
let source_eval_check = ref (fun ~k:_ ~pos:_ _ -> ())
let collect_after = ref (fun fn -> fn ())

module type Regexp_t = Regexp.T

let regexp = Regexp.regexp_ref
