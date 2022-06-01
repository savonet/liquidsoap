let type_of_encoder =
  ref (fun ~pos _ ->
      raise (Lang_error.Encoder_error (pos, "Encoders are not implemented!")))

type encoder_params =
  (string * [ `Value of Value.t | `Encoder of encoder ]) list

and encoder = string * encoder_params

let make_encoder =
  ref (fun ~pos _ _ ->
      raise (Lang_error.Encoder_error (pos, "Encoders are not implemented!")))

let has_encoder = ref (fun _ -> false)
let liq_libs_dir = ref (fun () -> raise Not_found)
let version = ref (fun () -> raise Not_found)
let log_path = ref None
let source_eval_check = ref (fun ~k:_ ~pos:_ _ -> ())
