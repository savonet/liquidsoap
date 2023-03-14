(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

(** SRT input *)

open Unsigned

exception Done
exception Not_connected

module Socket_value = struct
  let read_only_socket_options_specs = [("read_data", `Int Srt.rcvdata)]

  let read_write_socket_options_specs =
    [
      ("read_timeout", `Int Srt.rcvtimeo);
      ("write_timeout", `Int Srt.sndtimeo);
      ("streamid", `String Srt.streamid);
      ("pbkeylen", `Int Srt.pbkeylen);
      ("read_latency", `Int Srt.rcvlatency);
    ]

  let mk_socket_option name socket_opt =
    let t =
      match socket_opt with
        | `Int _ -> Lang.int_t
        | `Bool _ -> Lang.bool_t
        | `String _ -> Lang.string_t
    in
    ( name,
      ([], Lang.fun_t [] t),
      "Get " ^ name ^ " option",
      fun s ->
        Lang.val_fun [] (fun _ ->
            try
              match socket_opt with
                | `Int socket_opt -> Lang.int (Srt.getsockflag s socket_opt)
                | `Bool socket_opt -> Lang.bool (Srt.getsockflag s socket_opt)
                | `String socket_opt ->
                    Lang.string (Srt.getsockflag s socket_opt)
            with exn ->
              let bt = Printexc.get_raw_backtrace () in
              Lang.raise_as_runtime ~bt ~kind:"srt" exn) )

  let socket_options_meths =
    List.fold_left
      (fun cur (name, socket_opt) -> mk_socket_option name socket_opt :: cur)
      (List.fold_left
         (fun cur (name, socket_opt) -> mk_socket_option name socket_opt :: cur)
         [] read_only_socket_options_specs)
      read_write_socket_options_specs

  let stats_specs =
    [
      ( "msTimeStamp",
        Lang.int_t,
        fun v -> Lang.int (Int64.to_int v.Srt.Stats.msTimeStamp) );
      ( "pktSentTotal",
        Lang.int_t,
        fun v -> Lang.int (Int64.to_int v.Srt.Stats.pktSentTotal) );
      ( "pktRecvTotal",
        Lang.int_t,
        fun v -> Lang.int (Int64.to_int v.Srt.Stats.pktRecvTotal) );
      ( "pktSndLossTotal",
        Lang.int_t,
        fun v -> Lang.int v.Srt.Stats.pktSndLossTotal );
      ( "pktRcvLossTotal",
        Lang.int_t,
        fun v -> Lang.int v.Srt.Stats.pktRcvLossTotal );
      ( "pktRetransTotal",
        Lang.int_t,
        fun v -> Lang.int v.Srt.Stats.pktRetransTotal );
      ( "pktSentACKTotal",
        Lang.int_t,
        fun v -> Lang.int v.Srt.Stats.pktSentACKTotal );
      ( "pktRecvACKTotal",
        Lang.int_t,
        fun v -> Lang.int v.Srt.Stats.pktRecvACKTotal );
      ( "pktSentNAKTotal",
        Lang.int_t,
        fun v -> Lang.int v.Srt.Stats.pktSentNAKTotal );
      ( "pktRecvNAKTotal",
        Lang.int_t,
        fun v -> Lang.int v.Srt.Stats.pktRecvNAKTotal );
      ( "usSndDurationTotal",
        Lang.int_t,
        fun v -> Lang.int (Int64.to_int v.Srt.Stats.usSndDurationTotal) );
      ( "pktSndDropTotal",
        Lang.int_t,
        fun v -> Lang.int v.Srt.Stats.pktSndDropTotal );
      ( "pktRcvDropTotal",
        Lang.int_t,
        fun v -> Lang.int v.Srt.Stats.pktRcvDropTotal );
      ( "pktRcvUndecryptTotal",
        Lang.int_t,
        fun v -> Lang.int v.Srt.Stats.pktRcvUndecryptTotal );
      ( "byteSentTotal",
        Lang.int_t,
        fun v -> Lang.int (UInt64.to_int v.Srt.Stats.byteSentTotal) );
      ( "byteRecvTotal",
        Lang.int_t,
        fun v -> Lang.int (UInt64.to_int v.Srt.Stats.byteRecvTotal) );
      ( "byteRetransTotal",
        Lang.int_t,
        fun v -> Lang.int (UInt64.to_int v.Srt.Stats.byteRetransTotal) );
      ( "byteSndDropTotal",
        Lang.int_t,
        fun v -> Lang.int (UInt64.to_int v.Srt.Stats.byteSndDropTotal) );
      ( "byteRcvDropTotal",
        Lang.int_t,
        fun v -> Lang.int (UInt64.to_int v.Srt.Stats.byteRcvDropTotal) );
      ( "byteRcvUndecryptTotal",
        Lang.int_t,
        fun v -> Lang.int (UInt64.to_int v.Srt.Stats.byteRcvUndecryptTotal) );
      ( "pktSent",
        Lang.int_t,
        fun v -> Lang.int (Int64.to_int v.Srt.Stats.pktSent) );
      ( "pktRecv",
        Lang.int_t,
        fun v -> Lang.int (Int64.to_int v.Srt.Stats.pktRecv) );
      ("pktSndLoss", Lang.int_t, fun v -> Lang.int v.Srt.Stats.pktSndLoss);
      ("pktRcvLoss", Lang.int_t, fun v -> Lang.int v.Srt.Stats.pktRcvLoss);
      ("pktRetrans", Lang.int_t, fun v -> Lang.int v.Srt.Stats.pktRetrans);
      ("pktRcvRetrans", Lang.int_t, fun v -> Lang.int v.Srt.Stats.pktRcvRetrans);
      ("pktSentACK", Lang.int_t, fun v -> Lang.int v.Srt.Stats.pktSentACK);
      ("pktRecvACK", Lang.int_t, fun v -> Lang.int v.Srt.Stats.pktRecvACK);
      ("pktSentNAK", Lang.int_t, fun v -> Lang.int v.Srt.Stats.pktSentNAK);
      ("pktRecvNAK", Lang.int_t, fun v -> Lang.int v.Srt.Stats.pktRecvNAK);
      ( "mbpsSendRate",
        Lang.float_t,
        fun v -> Lang.float v.Srt.Stats.mbpsSendRate );
      ( "mbpsRecvRate",
        Lang.float_t,
        fun v -> Lang.float v.Srt.Stats.mbpsRecvRate );
      ( "usSndDuration",
        Lang.int_t,
        fun v -> Lang.int (Int64.to_int v.Srt.Stats.usSndDuration) );
      ( "pktReorderDistance",
        Lang.int_t,
        fun v -> Lang.int v.Srt.Stats.pktReorderDistance );
      ( "pktRcvAvgBelatedTime",
        Lang.float_t,
        fun v -> Lang.float v.Srt.Stats.pktRcvAvgBelatedTime );
      ( "pktRcvBelated",
        Lang.int_t,
        fun v -> Lang.int (Int64.to_int v.Srt.Stats.pktRcvBelated) );
      ("pktSndDrop", Lang.int_t, fun v -> Lang.int v.Srt.Stats.pktSndDrop);
      ("pktRcvDrop", Lang.int_t, fun v -> Lang.int v.Srt.Stats.pktRcvDrop);
      ( "pktRcvUndecrypt",
        Lang.int_t,
        fun v -> Lang.int v.Srt.Stats.pktRcvUndecrypt );
      ( "byteSent",
        Lang.int_t,
        fun v -> Lang.int (UInt64.to_int v.Srt.Stats.byteSent) );
      ( "byteRecv",
        Lang.int_t,
        fun v -> Lang.int (UInt64.to_int v.Srt.Stats.byteRecv) );
      ( "byteRetrans",
        Lang.int_t,
        fun v -> Lang.int (UInt64.to_int v.Srt.Stats.byteRetrans) );
      ( "byteSndDrop",
        Lang.int_t,
        fun v -> Lang.int (UInt64.to_int v.Srt.Stats.byteSndDrop) );
      ( "byteRcvDrop",
        Lang.int_t,
        fun v -> Lang.int (UInt64.to_int v.Srt.Stats.byteRcvDrop) );
      ( "byteRcvUndecrypt",
        Lang.int_t,
        fun v -> Lang.int (UInt64.to_int v.Srt.Stats.byteRcvUndecrypt) );
      ( "usPktSndPeriod",
        Lang.float_t,
        fun v -> Lang.float v.Srt.Stats.usPktSndPeriod );
      ("pktFlowWindow", Lang.int_t, fun v -> Lang.int v.Srt.Stats.pktFlowWindow);
      ( "pktCongestionWindow",
        Lang.int_t,
        fun v -> Lang.int v.Srt.Stats.pktCongestionWindow );
      ("pktFlightSize", Lang.int_t, fun v -> Lang.int v.Srt.Stats.pktFlightSize);
      ("msRTT", Lang.float_t, fun v -> Lang.float v.Srt.Stats.msRTT);
      ( "mbpsBandwidth",
        Lang.float_t,
        fun v -> Lang.float v.Srt.Stats.mbpsBandwidth );
      ( "byteAvailSndBuf",
        Lang.int_t,
        fun v -> Lang.int v.Srt.Stats.byteAvailSndBuf );
      ( "byteAvailRcvBuf",
        Lang.int_t,
        fun v -> Lang.int v.Srt.Stats.byteAvailRcvBuf );
      ("mbpsMaxBW", Lang.float_t, fun v -> Lang.float v.Srt.Stats.mbpsMaxBW);
      ("byteMSS", Lang.int_t, fun v -> Lang.int v.Srt.Stats.byteMSS);
      ("pktSndBuf", Lang.int_t, fun v -> Lang.int v.Srt.Stats.pktSndBuf);
      ("byteSndBuf", Lang.int_t, fun v -> Lang.int v.Srt.Stats.byteSndBuf);
      ("msSndBuf", Lang.int_t, fun v -> Lang.int v.Srt.Stats.msSndBuf);
      ( "msSndTsbPdDelay",
        Lang.int_t,
        fun v -> Lang.int v.Srt.Stats.msSndTsbPdDelay );
      ("pktRcvBuf", Lang.int_t, fun v -> Lang.int v.Srt.Stats.pktRcvBuf);
      ("byteRcvBuf", Lang.int_t, fun v -> Lang.int v.Srt.Stats.byteRcvBuf);
      ("msRcvBuf", Lang.int_t, fun v -> Lang.int v.Srt.Stats.msRcvBuf);
      ( "msRcvTsbPdDelay",
        Lang.int_t,
        fun v -> Lang.int v.Srt.Stats.msRcvTsbPdDelay );
      ( "pktSndFilterExtraTotal",
        Lang.int_t,
        fun v -> Lang.int v.Srt.Stats.pktSndFilterExtraTotal );
      ( "pktRcvFilterExtraTotal",
        Lang.int_t,
        fun v -> Lang.int v.Srt.Stats.pktRcvFilterExtraTotal );
      ( "pktRcvFilterSupplyTotal",
        Lang.int_t,
        fun v -> Lang.int v.Srt.Stats.pktRcvFilterSupplyTotal );
      ( "pktRcvFilterLossTotal",
        Lang.int_t,
        fun v -> Lang.int v.Srt.Stats.pktRcvFilterLossTotal );
      ( "pktSndFilterExtra",
        Lang.int_t,
        fun v -> Lang.int v.Srt.Stats.pktSndFilterExtra );
      ( "pktRcvFilterExtra",
        Lang.int_t,
        fun v -> Lang.int v.Srt.Stats.pktRcvFilterExtra );
      ( "pktRcvFilterSupply",
        Lang.int_t,
        fun v -> Lang.int v.Srt.Stats.pktRcvFilterSupply );
      ( "pktRcvFilterLoss",
        Lang.int_t,
        fun v -> Lang.int v.Srt.Stats.pktRcvFilterLoss );
    ]

  let stats_t =
    Lang.record_t (List.map (fun (name, t, _) -> (name, t)) stats_specs)

  include Value.MkAbstract (struct
    type content = Srt.socket

    let name = "srt_socket"

    let to_json ~pos _ =
      Runtime_error.raise ~pos
        ~message:"SRT socket cannot be represented as json" "json"

    let descr _ = "<srt_socket>"
    let compare = Stdlib.compare
  end)

  let meths =
    socket_options_meths
    @ [
        ( "status",
          ([], Lang.fun_t [] Lang.string_t),
          "Socket status",
          fun s ->
            Lang.val_fun [] (fun _ ->
                Lang.string
                  (match Srt.getsockstate s with
                    | `Init -> "initialized"
                    | `Opened -> "opened"
                    | `Listening -> "listening"
                    | `Connecting -> "connecting"
                    | `Connected -> "connected"
                    | `Broken -> "broken"
                    | `Closing -> "closing"
                    | `Closed -> "closed"
                    | `Nonexist -> "non_existant")) );
        ( "close",
          ([], Lang.fun_t [] Lang.unit_t),
          "Close socket",
          fun s ->
            Lang.val_fun [] (fun _ ->
                Srt.close s;
                Lang.unit) );
        ( "bstats",
          ([], Lang.fun_t [(true, "clear", Lang.nullable_t Lang.bool_t)] stats_t),
          "Socket bstats",
          fun s ->
            Lang.val_fun
              [("clear", "clear", Some Lang.null)]
              (fun p ->
                let clear =
                  Lang.to_valued_option Lang.to_bool (List.assoc "clear" p)
                in
                let stats = Srt.Stats.bstats ?clear s in
                Lang.record
                  (List.map (fun (n, _, fn) -> (n, fn stats)) stats_specs)) );
        ( "bistats",
          ( [],
            Lang.fun_t
              [
                (true, "clear", Lang.nullable_t Lang.bool_t);
                (true, "instantaneous", Lang.nullable_t Lang.bool_t);
              ]
              stats_t ),
          "Socket bstats",
          fun s ->
            Lang.val_fun
              [
                ("clear", "clear", Some Lang.null);
                ("instantaneous", "instantaneous", Some Lang.null);
              ]
              (fun p ->
                let clear =
                  Lang.to_valued_option Lang.to_bool (List.assoc "clear" p)
                in
                let instantaneous =
                  Lang.to_valued_option Lang.to_bool
                    (List.assoc "instantaneous" p)
                in
                let stats = Srt.Stats.bistats ?clear ?instantaneous s in
                Lang.record
                  (List.map (fun (n, _, fn) -> (n, fn stats)) stats_specs)) );
      ]

  let t =
    Lang.method_t t (List.map (fun (lbl, t, descr, _) -> (lbl, t, descr)) meths)

  let to_value s =
    Lang.meth (to_value s) (List.map (fun (lbl, _, _, m) -> (lbl, m s)) meths)
end
