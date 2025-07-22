include Liquidsoap_json.Json

let sedlexing_error ~pos message =
  let message = Printf.sprintf "In json data %s" message in
  Runtime_error.raise ~message ~pos "json"

let from_string ?(pos = []) ?json5 s =
  try from_string ?json5 s with
    | Runtime_error.Runtime_error _ as exn ->
        let bt = Printexc.get_raw_backtrace () in
        Printexc.raise_with_backtrace exn bt
    | Parse_error { pos = lexbuf_pos; message } ->
        let message =
          Printf.sprintf "In json data %s: %s"
            Pos.(to_string (of_lexing_pos lexbuf_pos))
            message
        in
        Runtime_error.raise ~message ~pos "json"
    | Sedlexing.InvalidCodepoint c ->
        sedlexing_error ~pos (Printf.sprintf "Invalid codepoint: %d" c)
    | Sedlexing.MalFormed -> sedlexing_error ~pos "Malformed input"
    | _ -> Runtime_error.raise ~message:"Parse error" ~pos "json"

let to_string ?compact ?json5 v =
  try to_string ?compact ?json5 v with
    | Infinite ->
        Runtime_error.raise ~pos:[]
          ~message:
            "Infinite numbers cannot be represented in JSON. You might want to \
             consider using the `json5` representation."
          "json"
    | NaN ->
        Runtime_error.raise ~pos:[]
          ~message:
            "NaN numbers cannot be represented in JSON. You might want to \
             consider using the `json5` representation."
          "json"
