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
    | "0" -> INT_0
    | "1" -> INT_1
    | "2" -> INT_2
    | "3" -> INT_3
    | "4" -> INT_4
    | "5" -> INT_5
    | "6" -> INT_6
    | "7" -> INT_7
    | "8" -> INT_8
    | "9" -> INT_9
    | "10" -> INT_10
    | "11" -> INT_11
    | "12" -> INT_12
    | "13" -> INT_13
    | "14" -> INT_14
    | "15" -> INT_15
    | "16" -> INT_16
    | "17" -> INT_17
    | "18" -> INT_18
    | "19" -> INT_19
    | "20" -> INT_20
    | "21" -> INT_21
    | "22" -> INT_22
    | "23" -> INT_23
    | "24" -> INT_24
    | "25" -> INT_25
    | "26" -> INT_26
    | "27" -> INT_27
    | "28" -> INT_28
    | "29" -> INT_29
    | "30" -> INT_30
    | "31" -> INT_31
    | "32" -> INT_32
    | "33" -> INT_33
    | "34" -> INT_34
    | "35" -> INT_35
    | "36" -> INT_36
    | "37" -> INT_37
    | "38" -> INT_38
    | "39" -> INT_39
    | "40" -> INT_40
    | "41" -> INT_41
    | "42" -> INT_42
    | "43" -> INT_43
    | "44" -> INT_44
    | "45" -> INT_45
    | "46" -> INT_46
    | "47" -> INT_47
    | "48" -> INT_48
    | "49" -> INT_49
    | "50" -> INT_50
    | "51" -> INT_51
    | "52" -> INT_52
    | "53" -> INT_53
    | "54" -> INT_54
    | "55" -> INT_55
    | "56" -> INT_56
    | "57" -> INT_57
    | "58" -> INT_58
    | "59" -> INT_59
    | "@yearly" -> YEARLY
    | "@annually" -> YEARLY
    | "@monthly" -> MONTHLY
    | "@weekly" -> WEEKLY
    | "@daily" -> DAILY
    | "@hourly" -> HOURLY
    | eof -> EOF
    | _ ->
        raise
          (Cron_base.Parse_error
             ( Sedlexing.lexing_bytes_positions lexbuf,
               Printf.sprintf "Invalid CRON expression!" ))
