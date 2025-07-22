include Cron_base

let test entry =
  let {
    Unix.tm_min = sys_minute;
    tm_hour = sys_hour;
    tm_mday = sys_month_day;
    tm_mon = sys_month;
    tm_wday = sys_week_day;
  } =
    Unix.(localtime (time ()))
  in
  match entry with
    | { minute = `Int v } when sys_minute <> v -> false
    | { minute = `Range (v, v') } when sys_minute < v || v' < sys_minute ->
        false
    | { minute = `Step v } when sys_minute mod v <> 0 -> false
    | { hour = `Int v } when sys_hour <> v -> false
    | { hour = `Range (v, v') } when sys_hour < v || v' < sys_hour -> false
    | { hour = `Step v } when sys_hour mod v <> 0 -> false
    | { month_day = `Int v } when sys_month_day <> v -> false
    | { month_day = `Range (v, v') }
      when sys_month_day < v || v' < sys_month_day ->
        false
    | { month_day = `Step v } when sys_month_day mod v <> 0 -> false
    | { month = `Int v } when sys_month <> v -> false
    | { month = `Range (v, v') } when sys_month < v || v' < sys_month -> false
    | { month = `Step v } when sys_month mod v <> 0 -> false
    | { week_day = `Int v } when sys_week_day <> v -> false
    | { week_day = `Range (v, v') } when sys_week_day < v || v' < sys_week_day
      ->
        false
    | { week_day = `Step v } when sys_week_day mod v <> 0 -> false
    | _ -> true

let parse =
  let processor =
    MenhirLib.Convert.Simplified.traditional2revised Cron_parser.cron
  in
  fun s ->
    let lexbuf = Sedlexing.Utf8.from_string s in
    processor (fun () ->
        let token = Cron_lexer.token lexbuf in
        let p, p' = Sedlexing.lexing_bytes_positions lexbuf in
        (token, p, p'))
