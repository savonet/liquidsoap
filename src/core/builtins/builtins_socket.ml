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
    type content = Http.socket

    let name = "socket"

    let to_json ~pos _ =
      Lang.raise_error ~pos ~message:"Socket cannot be represented as json"
        "json"

    let descr s = Printf.sprintf "<%s socket>" s#typ
    let compare = Stdlib.compare
  end)

  let meths =
    [
      ( "type",
        ([], Lang.string_t),
        "Socket type",
        fun (socket : content) -> Lang.string socket#typ );
      ( "write",
        ( [],
          Lang.fun_t
            [
              (true, "timeout", Lang.nullable_t Lang.float_t);
              (false, "", Lang.string_t);
            ]
            Lang.unit_t ),
        "Write data to a socket",
        fun socket ->
          Lang.val_fun
            [("timeout", "timeout", Some (Lang.float 10.)); ("", "", None)]
            (fun p ->
              let timeout =
                Lang.to_valued_option Lang.to_float (List.assoc "timeout" p)
              in
              let data = Lang.to_string (List.assoc "" p) in
              let data = Bytes.of_string data in
              let len = Bytes.length data in
              let start_time = Unix.gettimeofday () in
              let check_timeout () =
                match timeout with
                  | None -> ()
                  | Some t -> (
                      let rem = start_time +. t -. Unix.gettimeofday () in
                      try
                        if rem <= 0. then failwith "timeout!";
                        socket#wait_for `Write rem
                      with _ ->
                        Lang.raise_error ~pos:(Lang.pos p)
                          ~message:"Timeout while writing to the socket!"
                          "socket")
              in
              try
                let rec f pos =
                  check_timeout ();
                  let n = socket#write data pos (len - pos) in
                  if n < len then f (pos + n)
                in
                f 0;
                Lang.unit
              with exn ->
                let bt = Printexc.get_raw_backtrace () in
                Lang.raise_as_runtime ~bt ~kind:"socket" exn) );
      ( "read",
        ( [],
          Lang.fun_t
            [(true, "timeout", Lang.nullable_t Lang.float_t)]
            Lang.string_t ),
        "Read data from a socket. Reading is done when the function returns an \
         empty string `\"\"`.",
        fun socket ->
          let buflen = Utils.pagesize in
          let buf = Bytes.create buflen in
          Lang.val_fun
            [("timeout", "timeout", Some (Lang.float 10.))]
            (fun p ->
              let timeout =
                Lang.to_valued_option Lang.to_float (List.assoc "timeout" p)
              in
              let start_time = Unix.gettimeofday () in
              let check_timeout () =
                match timeout with
                  | None -> ()
                  | Some t -> (
                      let rem = start_time +. t -. Unix.gettimeofday () in
                      try
                        if rem <= 0. then failwith "timeout!";
                        socket#wait_for `Read rem
                      with _ ->
                        Lang.raise_error ~pos:(Lang.pos p)
                          ~message:"Timeout while reading from the socket!"
                          "socket")
              in
              try
                check_timeout ();
                let n = socket#read buf 0 buflen in
                Lang.string (Bytes.sub_string buf 0 n)
              with exn ->
                let bt = Printexc.get_raw_backtrace () in
                Lang.raise_as_runtime ~bt ~kind:"socket" exn) );
      ( "close",
        ([], Lang.fun_t [] Lang.unit_t),
        "Close the socket.",
        fun socket ->
          Lang.val_fun [] (fun _ ->
              try
                socket#close;
                Lang.unit
              with exn ->
                let bt = Printexc.get_raw_backtrace () in
                Lang.raise_as_runtime ~bt ~kind:"socket" exn) );
    ]

  let t =
    Lang.method_t t (List.map (fun (lbl, t, descr, _) -> (lbl, t, descr)) meths)

  let to_value socket =
    Lang.meth (to_value socket)
      (List.map (fun (lbl, _, _, m) -> (lbl, m socket)) meths)

  let of_unix_file_descr = Http.unix_socket
  let of_value socket = of_value (Lang.demeth socket)
end
