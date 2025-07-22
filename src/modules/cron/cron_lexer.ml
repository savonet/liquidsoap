open Cron_parser

let decimal_digit = [%sedlex.regexp? '0' .. '9']
let one_nine = [%sedlex.regexp? '1' .. '9']
let non_null_integer = [%sedlex.regexp? one_nine, Star decimal_digit]
let integer = [%sedlex.regexp? '0' | non_null_integer]
let skipped = [%sedlex.regexp? Plus ' ']

let rec token lexbuf =
  match%sedlex lexbuf with
    | skipped -> token lexbuf
    | "-" -> DASH
    | "/" -> SLASH
    | "*" -> STAR
    | "," -> COMMA
    | "sun" -> WEEK_DAY 0
    | "mon" -> WEEK_DAY 1
    | "tue" -> WEEK_DAY 2
    | "wed" -> WEEK_DAY 3
    | "thu" -> WEEK_DAY 4
    | "fri" -> WEEK_DAY 5
    | "sat" -> WEEK_DAY 6
    | "jan" -> MONTH 1
    | "feb" -> MONTH 2
    | "mar" -> MONTH 3
    | "apr" -> MONTH 4
    | "may" -> MONTH 5
    | "jun" -> MONTH 6
    | "jul" -> MONTH 7
    | "aug" -> MONTH 8
    | "sep" -> MONTH 9
    | "oct" -> MONTH 10
    | "nov" -> MONTH 11
    | "dec" -> MONTH 12
    | "@yearly" -> YEARLY
    | "@annually" -> YEARLY
    | "@monthly" -> MONTHLY
    | "@weekly" -> WEEKLY
    | "@daily" -> DAILY
    | "@hourly" -> HOURLY
    | eof -> EOF
    | integer -> INT (int_of_string (Sedlexing.Utf8.lexeme lexbuf))
    | _ -> raise (Cron_base.Parse_error "Syntax error")
