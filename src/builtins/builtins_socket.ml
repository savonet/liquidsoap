(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

module SocketValue = struct
  include Value.MkAbstract (struct
    type content = Unix.file_descr

    let name = "socket"

    let to_json ~pos _ =
      Runtime_error.raise ~pos ~message:"Socket cannot be represented as json"
        "json"

    let descr _ = "<socket>"
    let compare = Stdlib.compare
  end)

  let meths =
    [
      ( "write",
        ([], Lang.fun_t [(false, "", Lang.string_t)] Lang.unit_t),
        "Write data to a socket",
        fun fd ->
          Lang.val_fun
            [("", "", None)]
            (fun p ->
              let data = Lang.to_string (List.assoc "" p) in
              let data = Bytes.of_string data in
              let len = Bytes.length data in
              try
                let rec f pos =
                  let n = Unix.write fd data pos (len - pos) in
                  if n < len then f (pos + n)
                in
                f 0;
                Lang.unit
              with exn ->
                let bt = Printexc.get_raw_backtrace () in
                Lang.raise_as_runtime ~bt ~kind:"socket" exn) );
      ( "read",
        ([], Lang.fun_t [] Lang.string_t),
        "Read data from a socket. Reading is done when the function returns an \
         empty srring `\"\"`.",
        fun fd ->
          let buflen = Utils.pagesize in
          let buf = Bytes.create buflen in
          Lang.val_fun [] (fun _ ->
              try
                let n = Unix.read fd buf 0 buflen in
                Lang.string (Bytes.sub_string buf 0 n)
              with exn ->
                let bt = Printexc.get_raw_backtrace () in
                Lang.raise_as_runtime ~bt ~kind:"socket" exn) );
      ( "close",
        ([], Lang.fun_t [] Lang.unit_t),
        "Close the socket.",
        fun fd ->
          Lang.val_fun [] (fun _ ->
              try
                Unix.close fd;
                Lang.unit
              with exn ->
                let bt = Printexc.get_raw_backtrace () in
                Lang.raise_as_runtime ~bt ~kind:"socket" exn) );
    ]

  let t =
    Lang.method_t t (List.map (fun (lbl, t, descr, _) -> (lbl, t, descr)) meths)

  let to_value err =
    Lang.meth (to_value err)
      (List.map (fun (lbl, _, _, m) -> (lbl, m err)) meths)

  let of_value err = of_value (Lang.demeth err)
end
