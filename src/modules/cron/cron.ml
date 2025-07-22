include Base

let valid_entry (v, v') =
  match v' with
    | `Int v' -> v = v'
    | `List l -> List.mem v l
    | `Step s -> v mod s == 0
    | `Range (min, max) -> min <= v && v <= max
    | `Any -> true

let test ?time entry =
  let time = match time with None -> Unix.time () | Some t -> t in
  let {
    Unix.tm_min = sys_minute;
    tm_hour = sys_hour;
    tm_mday = sys_month_day;
    tm_mon = sys_month;
    tm_wday = sys_week_day;
  } =
    Unix.localtime time
  in
  let { minute; hour; month_day; month; week_day } = entry in
  List.for_all valid_entry
    [
      (sys_minute, minute);
      (sys_hour, hour);
      (sys_month_day, month_day);
      (sys_month, month);
      (sys_week_day, week_day);
    ]

let parse =
  let processor =
    MenhirLib.Convert.Simplified.traditional2revised Parser.cron
  in
  fun s ->
    let lexbuf = Sedlexing.Utf8.from_string s in
    try
      processor (fun () ->
          let token = Lexer.token lexbuf in
          let p, p' = Sedlexing.lexing_bytes_positions lexbuf in
          (token, p, p'))
    with
      | Parse_error _ as exn ->
          let bt = Printexc.get_raw_backtrace () in
          Printexc.raise_with_backtrace exn bt
      | _ -> raise (Parse_error "Invalid CRON string")
