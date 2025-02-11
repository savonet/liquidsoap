(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

let _ =
  let t = ("", Lang.int_t, None, None) in
  Lang.add_builtin "time_in_mod" ~category:`Time ~flags:[`Hidden]
    ~descr:
      ("INTERNAL: time_in_mod(a,b,c) checks that the unix time T "
     ^ "satisfies a <= T mod c < b") [t; t; t] Lang.bool_t (fun p ->
      let g pos = Lang.to_int (Lang.assoc "" pos p) in
      let a, b, c = (g 1, g 2, g 3) in
      let t = Unix.localtime (Unix.time ()) in
      let t =
        t.Unix.tm_sec + (t.Unix.tm_min * 60)
        + (t.Unix.tm_hour * 60 * 60)
        + (t.Unix.tm_wday * 24 * 60 * 60)
      in
      let t = t mod c in
      if a <= b then Lang.bool (a <= t && t < b)
      else Lang.bool (not (b <= t && t < a)))

let time =
  Lang.add_builtin ~category:`Time "time"
    ~descr:
      "Return the current time since 00:00:00 GMT, Jan. 1, 1970, in seconds."
    [] Lang.float_t (fun _ -> Lang.float (Unix.gettimeofday ()))

let _ =
  Lang.add_builtin ~category:`Time ~base:time "up"
    ~descr:"Current time, in seconds, since the script has started." []
    Lang.float_t (fun _ -> Lang.float (Utils.uptime ()))

let _ =
  let time_t =
    Lang.method_t Lang.unit_t
      [
        ("sec", ([], Lang.int_t), "Seconds.");
        ("min", ([], Lang.int_t), "Minutes.");
        ("hour", ([], Lang.int_t), "Hours.");
        ("day", ([], Lang.int_t), "Day of month.");
        ("month", ([], Lang.int_t), "Month of year.");
        ("year", ([], Lang.int_t), "Year.");
        ( "week_day",
          ([], Lang.int_t),
          "Day of week (Sunday is 0 or 7, Saturday is 6)." );
        ("year_day", ([], Lang.int_t), "Day of year, between `1` and `366`.");
        ("dst", ([], Lang.bool_t), "Daylight time savings in effect.");
      ]
  in
  let return tm =
    Lang.record
      [
        ("sec", Lang.int tm.Unix.tm_sec);
        ("min", Lang.int tm.Unix.tm_min);
        ("hour", Lang.int tm.Unix.tm_hour);
        ("day", Lang.int tm.Unix.tm_mday);
        ("month", Lang.int (1 + tm.Unix.tm_mon));
        ("year", Lang.int (1900 + tm.Unix.tm_year));
        ("week_day", Lang.int tm.Unix.tm_wday);
        ("year_day", Lang.int (1 + tm.Unix.tm_yday));
        ("dst", Lang.bool tm.Unix.tm_isdst);
      ]
  in
  let nullable_time v =
    match Lang.to_option v with
      | Some v -> Lang.to_float v
      | None -> Unix.gettimeofday ()
  in
  let descr tz =
    Printf.sprintf
      "Convert a time in seconds into a date in the %s time zone (current time \
       is used if no argument is provided)."
      tz
  in
  ignore
    (Lang.add_builtin ~category:`Time ~base:time "local" ~descr:(descr "local")
       [("", Lang.nullable_t Lang.float_t, Some Lang.null, None)]
       time_t
       (fun p ->
         let t = nullable_time (List.assoc "" p) in
         return (Unix.localtime t)));
  ignore
    (Lang.add_builtin ~category:`Time ~base:time "utc" ~descr:(descr "UTC")
       [("", Lang.nullable_t Lang.float_t, Some Lang.null, None)]
       time_t
       (fun p ->
         let t = nullable_time (List.assoc "" p) in
         return (Unix.gmtime t)))

let _ =
  let time_t =
    Lang.method_t Lang.unit_t
      [
        ("sec", ([], Lang.int_t), "Seconds.");
        ("min", ([], Lang.int_t), "Minutes.");
        ("hour", ([], Lang.int_t), "Hours.");
        ("day", ([], Lang.int_t), "Day of month.");
        ("month", ([], Lang.int_t), "Month of year.");
        ("year", ([], Lang.int_t), "Year.");
        ( "dst",
          ([], Lang.nullable_t Lang.bool_t),
          "Daylight time savings in effect." );
      ]
  in
  Lang.add_builtin ~category:`Time ~base:time "make"
    ~descr:
      "Convert a date and time in the local timezone into a time, in seconds, \
       since 00:00:00 GMT, Jan. 1, 1970."
    [("", time_t, None, None)]
    Lang.float_t
    (fun p ->
      let tm = List.assoc "" p in
      let tm =
        {
          Utils.tm_sec = Lang.to_int (Value.invoke tm "sec");
          tm_min = Lang.to_int (Value.invoke tm "min");
          tm_hour = Lang.to_int (Value.invoke tm "hour");
          tm_mday = Lang.to_int (Value.invoke tm "day");
          tm_mon = Lang.to_int (Value.invoke tm "month") - 1;
          tm_year = Lang.to_int (Value.invoke tm "year") - 1900;
          tm_isdst = Lang.to_valued_option Lang.to_bool (Value.invoke tm "dst");
        }
      in
      Lang.float (Utils.mktime tm))

let _ =
  let processor =
    MenhirLib.Convert.Simplified.traditional2revised
      Liquidsoap_lang.Parser.time_predicate
  in
  Lang.add_builtin ~category:`Time ~base:time "predicate"
    ~descr:"Parse a string as a time predicate"
    [("", Lang.string_t, None, None)]
    (Lang.fun_t [] Lang.bool_t)
    (fun p ->
      let v = List.assoc "" p in
      let predicate = Lang.to_string v in
      let lexbuf = Sedlexing.Utf8.from_string predicate in
      try
        let tokenizer = Liquidsoap_lang.Preprocessor.mk_tokenizer lexbuf in
        let predicate =
          Liquidsoap_lang.Term_reducer.to_term (processor tokenizer)
        in
        Lang.val_fun [] (fun _ -> Liquidsoap_lang.Evaluation.eval predicate)
      with _ ->
        Lang.raise_error
          ~message:
            (Printf.sprintf "Failed to parse %s as time predicate" predicate)
          ~pos:(Lang.pos p) "string")

let _ =
  let tz_t =
    Lang.method_t Lang.string_t
      [
        ("daylight", ([], Lang.string_t), "Daylight Savings Time");
        ( "utc_diff",
          ([], Lang.int_t),
          "Difference in seconds between the current timezone and UTC." );
      ]
  in
  Lang.add_builtin ~category:`Time ~base:time "zone"
    ~descr:"Returns a description of the time zone set for the running process."
    [] tz_t (fun _ ->
      let std, dst = Utils.timezone_by_name () in
      let tz = Utils.timezone () in
      Lang.meth (Lang.string std)
        [("daylight", Lang.string dst); ("utc_diff", Lang.int tz)])

let _ =
  Lang.add_builtin ~category:`Time ~base:time "string"
    ~descr:
      "Obtain a string representation of the current time. It takes a string \
       as argument where special strings are replaced roughly following \
       [strftime](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes): \
       %H is replaced by the current hour, %M minute, %S second, %A week day \
       (%a week day abbreviated), %d month day, %B month name (%b month name \
       abbreviated), %z timezone."
    ~examples:
      [
        {|s = time.string("Current time is %H:%M.")
print(s)|};
        {|# Backup a source naming the file based on time
output.file({time.string("/path/to/file%H%M%S.wav")}, ...)|};
      ]
    [
      ( "time",
        Lang.nullable_t Lang.float_t,
        Some Lang.null,
        Some
          "If specified convert the given time (in seconds since 00:00:00 GMT, \
           Jan. 1, 1970) instead of the current time." );
      ( "",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some
          "Description of the string to produce, e.g. `\"Current time is \
           %H:%M`\"`." );
    ]
    Lang.string_t
    (fun p ->
      let time =
        List.assoc "time" p |> Lang.to_option |> Option.map Lang.to_float
      in
      let s =
        List.assoc "" p |> Lang.to_option |> Option.map Lang.to_string
        |> Option.value ~default:"%A, %d %B %Y %H:%M:%S"
      in
      Lang.string (Utils.strftime ?time s))
