(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2020 Savonet team

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

open Lang_builtins

let () =
  let t = ("", Lang.int_t, None, None) in
  add_builtin "time_in_mod" ~cat:Other ~flags:[Lang.Hidden]
    ~descr:
      ( "INTERNAL: time_in_mod(a,b,c) checks that the unix time T "
      ^ "satisfies a <= T mod c < b" ) [t; t; t] Lang.bool_t (fun p ->
      match List.map (fun (_, x) -> Lang.to_int x) p with
        | [a; b; c] ->
            let t = Unix.localtime (Unix.time ()) in
            let t =
              t.Unix.tm_sec + (t.Unix.tm_min * 60)
              + (t.Unix.tm_hour * 60 * 60)
              + (t.Unix.tm_wday * 24 * 60 * 60)
            in
            let t = t mod c in
            if a <= b then Lang.bool (a <= t && t < b)
            else Lang.bool (not (b <= t && t < a))
        | _ -> assert false)

let () =
  add_builtin ~cat:Sys "time"
    ~descr:
      "Return the current time since 00:00:00 GMT, Jan. 1, 1970, in seconds." []
    Lang.float_t (fun _ -> Lang.float (Unix.gettimeofday ()))

let () =
  let time_t =
    Lang.record_t
      [
        ("sec", Lang.int_t);
        ("min", Lang.int_t);
        ("hour", Lang.int_t);
        ("mday", Lang.int_t);
        ("mon", Lang.int_t);
        ("year", Lang.int_t);
        ("wday", Lang.int_t);
        ("yday", Lang.int_t);
        ("isdst", Lang.bool_t);
      ]
  in
  let return tm =
    Lang.record
      [
        ("sec", Lang.int tm.Unix.tm_sec);
        ("min", Lang.int tm.Unix.tm_min);
        ("hour", Lang.int tm.Unix.tm_hour);
        ("mday", Lang.int tm.Unix.tm_mday);
        ("mon", Lang.int tm.Unix.tm_mon);
        ("year", Lang.int tm.Unix.tm_year);
        ("wday", Lang.int tm.Unix.tm_wday);
        ("yday", Lang.int tm.Unix.tm_yday);
        ("isdst", Lang.bool tm.Unix.tm_isdst);
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
       is used if no argument is provided). Fields meaning same as POSIX's `tm \
       struct`. In particular, \"year\" is: year - 1900, i.e. 117 for 2017!"
      tz
  in
  add_builtin ~cat:Sys "time.local" ~descr:(descr "local")
    [("", Lang.nullable_t Lang.float_t, Some Lang.null, None)]
    time_t
    (fun p ->
      let t = nullable_time (List.assoc "" p) in
      return (Unix.localtime t));
  add_builtin ~cat:Sys "time.utc" ~descr:(descr "UTC")
    [("", Lang.nullable_t Lang.float_t, Some Lang.null, None)]
    time_t
    (fun p ->
      let t = nullable_time (List.assoc "" p) in
      return (Unix.gmtime t))
