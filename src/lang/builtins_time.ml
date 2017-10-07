(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2017 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

open Lang_builtins

let () =
  add_builtin ~cat:Sys "time" ~descr:"Return the current time since 00:00:00 GMT, Jan. 1, 1970, in seconds."
    [] Lang.float_t (fun _ ->
      Lang.float (Unix.gettimeofday ()));
  let execute cb tm =
    Lang.apply cb ~t:(Lang.univ_t 1)
      ["sec",Lang.int tm.Unix.tm_sec;
       "min",Lang.int tm.Unix.tm_min;
       "hour",Lang.int tm.Unix.tm_hour;
       "mday",Lang.int tm.Unix.tm_mday;
       "mon",Lang.int tm.Unix.tm_mon;
       "year",Lang.int tm.Unix.tm_year;
       "wday",Lang.int tm.Unix.tm_wday;
       "yday",Lang.int tm.Unix.tm_yday;
       "isdst",Lang.bool tm.Unix.tm_isdst]
  in
  let univ = Lang.univ_t 1 in
  let fn_t = Lang.fun_t
    [false,"sec",Lang.int_t;
     false,"min",Lang.int_t;
     false,"hour",Lang.int_t;
     false,"mday",Lang.int_t;
     false,"mon",Lang.int_t;
     false,"year",Lang.int_t;
     false,"wday",Lang.int_t;
     false,"yday",Lang.int_t;
     false,"isdst",Lang.bool_t] univ
  in
  add_builtin ~cat:Sys "localtime" ~descr:"Convert a time in seconds into a date in \
          the local time zone and execute passed callback with the result. Fields meaning \
          same as POSIX's @tm struct@. Warning: \"year\" is: year - 1900, i.e. 117 for 2017!"
    ["",Lang.float_t,None,None;
     "",fn_t,None,None]
    univ (fun p ->
      let tm =
        Unix.localtime
         (Lang.to_float (Lang.assoc "" 1 p))
      in
      let fn = Lang.assoc "" 2 p in
      execute fn tm);
  add_builtin ~cat:Sys "gmtime" ~descr:"Convert a time in seconds into a date in \
          the UTC time zone and execute passed callback with the result. Fields meaning \
          same as POSIX's @tm struct@. Warning: \"year\" is: year - 1900, i.e. 117 for 2017!"
    ["",Lang.float_t,None,None;
     "",fn_t,None,None]
    univ (fun p ->
      let tm =
        Unix.localtime
         (Lang.to_float (Lang.assoc "" 1 p))
      in
      let fn = Lang.assoc "" 2 p in
      execute fn tm)
