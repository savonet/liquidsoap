(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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
        | _ ->
            assert false)

let () =
  add_builtin ~cat:Sys "time"
    ~descr:
      "Return the current time since 00:00:00 GMT, Jan. 1, 1970, in seconds."
    [] Lang.float_t (fun _ -> Lang.float (Unix.gettimeofday ())) ;
  let t = Lang.univ_t () in
  let execute cb tm =
    Lang.apply cb ~t
      [ ("sec", Lang.int tm.Unix.tm_sec);
        ("min", Lang.int tm.Unix.tm_min);
        ("hour", Lang.int tm.Unix.tm_hour);
        ("mday", Lang.int tm.Unix.tm_mday);
        ("mon", Lang.int tm.Unix.tm_mon);
        ("year", Lang.int tm.Unix.tm_year);
        ("wday", Lang.int tm.Unix.tm_wday);
        ("yday", Lang.int tm.Unix.tm_yday);
        ("isdst", Lang.bool tm.Unix.tm_isdst) ]
  in
  let fn_t =
    Lang.fun_t
      [ (false, "sec", Lang.int_t);
        (false, "min", Lang.int_t);
        (false, "hour", Lang.int_t);
        (false, "mday", Lang.int_t);
        (false, "mon", Lang.int_t);
        (false, "year", Lang.int_t);
        (false, "wday", Lang.int_t);
        (false, "yday", Lang.int_t);
        (false, "isdst", Lang.bool_t) ]
      t
  in
  add_builtin ~cat:Sys "localtime"
    ~descr:
      "Convert a time in seconds into a date in the local time zone and \
       execute passed callback with the result. Fields meaning same as \
       POSIX's `tm struct`. Warning: \"year\" is: year - 1900, i.e. 117 for \
       2017!" [("", Lang.float_t, None, None); ("", fn_t, None, None)] t
    (fun p ->
      let tm = Unix.localtime (Lang.to_float (Lang.assoc "" 1 p)) in
      let fn = Lang.assoc "" 2 p in
      execute fn tm) ;
  add_builtin ~cat:Sys "gmtime"
    ~descr:
      "Convert a time in seconds into a date in the UTC time zone and execute \
       passed callback with the result. Fields meaning same as POSIX's `tm \
       struct`. Warning: \"year\" is: year - 1900, i.e. 117 for 2017!"
    [("", Lang.float_t, None, None); ("", fn_t, None, None)] t (fun p ->
      let tm = Unix.localtime (Lang.to_float (Lang.assoc "" 1 p)) in
      let fn = Lang.assoc "" 2 p in
      execute fn tm) ;
  add_builtin ~cat:Liq "source.time"
    ~descr:"Get a source's time, based on its assigned clock"
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    Lang.float_t
    (fun p ->
      let s = Lang.to_source (List.assoc "" p) in
      let ticks =
        if Source.Clock_variables.is_known s#clock then
          (Source.Clock_variables.get s#clock)#get_tick
        else 0
      in
      let frame_position = Lazy.force Frame.duration *. float ticks in
      let in_frame_position =
        Frame.seconds_of_master (Frame.position s#get_memo)
      in
      Lang.float (frame_position +. in_frame_position))
