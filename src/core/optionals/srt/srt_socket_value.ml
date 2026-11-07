(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

(** The script socket value (spec/socket-value.md). [to_value] gives a socket
    with its methods, typed [t]. *)

module Base = Value.MkCustom (struct
  type content = Srt.socket

  let name = "srt_socket"

  let to_json ~pos _ =
    Lang.raise_error ~pos ~message:"SRT socket cannot be represented as json"
      "json"

  let to_string _ = "<srt_socket>"
  let compare a b = Stdlib.compare (Srt.socket_id a) (Srt.socket_id b)
end)

let srt_call p fn =
  try fn ()
  with Srt.Error _ as exn ->
    let bt = Printexc.get_raw_backtrace () in
    Lang.raise_error ~bt ~pos:(Lang.pos p) ~message:(Printexc.to_string exn)
      "srt"

let getter name typ to_value opt =
  ( name,
    ([], Lang.fun_t [] typ),
    Printf.sprintf "Value of the `%s` socket option." name,
    fun socket ->
      Lang.val_fun [] (fun p ->
          srt_call p (fun () -> to_value (Srt.getsockflag socket opt))) )

let setter name typ of_value opt =
  ( "set_" ^ name,
    ([], Lang.fun_t [(false, "", typ)] Lang.unit_t),
    Printf.sprintf "Set the `%s` socket option." name,
    fun socket ->
      Lang.val_fun
        [("", "", None)]
        (fun p ->
          srt_call p (fun () ->
              Srt.setsockflag socket opt (of_value (List.assoc "" p)));
          Lang.unit) )

let int_opt = (Lang.int_t, Lang.int, Lang.to_int)
let bool_opt = (Lang.bool_t, Lang.bool, Lang.to_bool)
let string_opt = (Lang.string_t, Lang.string, Lang.to_string)
let read name (typ, to_value, _) opt = [getter name typ to_value opt]
let write name (typ, _, of_value) opt = [setter name typ of_value opt]

let read_write name (typ, to_value, of_value) opt =
  [getter name typ to_value opt; setter name typ of_value opt]

let option_methods =
  List.concat
    [
      read "rcvdata" int_opt Srt.rcvdata;
      write "messageapi" bool_opt Srt.messageapi;
      write "payloadsize" int_opt Srt.payloadsize;
      write "conntimeo" int_opt Srt.conntimeo;
      write "passphrase" string_opt Srt.passphrase;
      write "enforced_encryption" bool_opt Srt.enforced_encryption;
      read_write "rcvsyn" bool_opt Srt.rcvsyn;
      read_write "sndsyn" bool_opt Srt.sndsyn;
      read_write "rcvtimeout" int_opt Srt.rcvtimeo;
      read_write "sndtimeout" int_opt Srt.sndtimeo;
      read_write "reuseaddr" bool_opt Srt.reuseaddr;
      read_write "rcvbuf" int_opt Srt.rcvbuf;
      read_write "sndbuf" int_opt Srt.sndbuf;
      read_write "udp_rcvbuf" int_opt Srt.udp_rcvbuf;
      read_write "udp_sndbuf" int_opt Srt.udp_sndbuf;
      read_write "streamid" string_opt Srt.streamid;
      read_write "pbkeylen" int_opt Srt.pbkeylen;
      read_write "ipv6only" bool_opt Srt.ipv6only;
      read_write "rcvlatency" int_opt Srt.rcvlatency;
      read_write "peerlatency" int_opt Srt.peerlatency;
      read_write "latency" int_opt Srt.latency;
      read_write "tsbpdmode" bool_opt Srt.tsbpdmode;
    ]

let string_of_status = function
  | `Init -> "initialized"
  | `Opened -> "opened"
  | `Listening -> "listening"
  | `Connecting -> "connecting"
  | `Connected -> "connected"
  | `Broken -> "broken"
  | `Closing -> "closing"
  | `Closed -> "closed"
  | `Nonexist -> "non_existant"

let saturate_int64 v =
  if Int64.compare v (Int64.of_int max_int) > 0 then max_int
  else if Int64.compare v (Int64.of_int min_int) < 0 then min_int
  else Int64.to_int v

let saturate_uint64 v =
  let open Unsigned.UInt64 in
  if compare v (of_int Stdlib.max_int) > 0 then Stdlib.max_int else to_int v

let stats_fields =
  let open Srt.Stats in
  let int name get = (name, Lang.int_t, fun s -> Lang.int (get s)) in
  let i64 name get = int name (fun s -> saturate_int64 (get s)) in
  let u64 name get = int name (fun s -> saturate_uint64 (get s)) in
  let float name get = (name, Lang.float_t, fun s -> Lang.float (get s)) in
  [
    i64 "msTimeStamp" (fun s -> s.msTimeStamp);
    i64 "pktSentTotal" (fun s -> s.pktSentTotal);
    i64 "pktRecvTotal" (fun s -> s.pktRecvTotal);
    int "pktSndLossTotal" (fun s -> s.pktSndLossTotal);
    int "pktRcvLossTotal" (fun s -> s.pktRcvLossTotal);
    int "pktRetransTotal" (fun s -> s.pktRetransTotal);
    int "pktSentACKTotal" (fun s -> s.pktSentACKTotal);
    int "pktRecvACKTotal" (fun s -> s.pktRecvACKTotal);
    int "pktSentNAKTotal" (fun s -> s.pktSentNAKTotal);
    int "pktRecvNAKTotal" (fun s -> s.pktRecvNAKTotal);
    i64 "usSndDurationTotal" (fun s -> s.usSndDurationTotal);
    int "pktSndDropTotal" (fun s -> s.pktSndDropTotal);
    int "pktRcvDropTotal" (fun s -> s.pktRcvDropTotal);
    int "pktRcvUndecryptTotal" (fun s -> s.pktRcvUndecryptTotal);
    u64 "byteSentTotal" (fun s -> s.byteSentTotal);
    u64 "byteRecvTotal" (fun s -> s.byteRecvTotal);
    u64 "byteRetransTotal" (fun s -> s.byteRetransTotal);
    u64 "byteSndDropTotal" (fun s -> s.byteSndDropTotal);
    u64 "byteRcvDropTotal" (fun s -> s.byteRcvDropTotal);
    u64 "byteRcvUndecryptTotal" (fun s -> s.byteRcvUndecryptTotal);
    i64 "pktSent" (fun s -> s.pktSent);
    i64 "pktRecv" (fun s -> s.pktRecv);
    int "pktSndLoss" (fun s -> s.pktSndLoss);
    int "pktRcvLoss" (fun s -> s.pktRcvLoss);
    int "pktRetrans" (fun s -> s.pktRetrans);
    int "pktRcvRetrans" (fun s -> s.pktRcvRetrans);
    int "pktSentACK" (fun s -> s.pktSentACK);
    int "pktRecvACK" (fun s -> s.pktRecvACK);
    int "pktSentNAK" (fun s -> s.pktSentNAK);
    int "pktRecvNAK" (fun s -> s.pktRecvNAK);
    float "mbpsSendRate" (fun s -> s.mbpsSendRate);
    float "mbpsRecvRate" (fun s -> s.mbpsRecvRate);
    i64 "usSndDuration" (fun s -> s.usSndDuration);
    int "pktReorderDistance" (fun s -> s.pktReorderDistance);
    float "pktRcvAvgBelatedTime" (fun s -> s.pktRcvAvgBelatedTime);
    i64 "pktRcvBelated" (fun s -> s.pktRcvBelated);
    int "pktSndDrop" (fun s -> s.pktSndDrop);
    int "pktRcvDrop" (fun s -> s.pktRcvDrop);
    int "pktRcvUndecrypt" (fun s -> s.pktRcvUndecrypt);
    u64 "byteSent" (fun s -> s.byteSent);
    u64 "byteRecv" (fun s -> s.byteRecv);
    u64 "byteRetrans" (fun s -> s.byteRetrans);
    u64 "byteSndDrop" (fun s -> s.byteSndDrop);
    u64 "byteRcvDrop" (fun s -> s.byteRcvDrop);
    u64 "byteRcvUndecrypt" (fun s -> s.byteRcvUndecrypt);
    float "usPktSndPeriod" (fun s -> s.usPktSndPeriod);
    int "pktFlowWindow" (fun s -> s.pktFlowWindow);
    int "pktCongestionWindow" (fun s -> s.pktCongestionWindow);
    int "pktFlightSize" (fun s -> s.pktFlightSize);
    float "msRTT" (fun s -> s.msRTT);
    float "mbpsBandwidth" (fun s -> s.mbpsBandwidth);
    int "byteAvailSndBuf" (fun s -> s.byteAvailSndBuf);
    int "byteAvailRcvBuf" (fun s -> s.byteAvailRcvBuf);
    float "mbpsMaxBW" (fun s -> s.mbpsMaxBW);
    int "byteMSS" (fun s -> s.byteMSS);
    int "pktSndBuf" (fun s -> s.pktSndBuf);
    int "byteSndBuf" (fun s -> s.byteSndBuf);
    int "msSndBuf" (fun s -> s.msSndBuf);
    int "msSndTsbPdDelay" (fun s -> s.msSndTsbPdDelay);
    int "pktRcvBuf" (fun s -> s.pktRcvBuf);
    int "byteRcvBuf" (fun s -> s.byteRcvBuf);
    int "msRcvBuf" (fun s -> s.msRcvBuf);
    int "msRcvTsbPdDelay" (fun s -> s.msRcvTsbPdDelay);
    int "pktSndFilterExtraTotal" (fun s -> s.pktSndFilterExtraTotal);
    int "pktRcvFilterExtraTotal" (fun s -> s.pktRcvFilterExtraTotal);
    int "pktRcvFilterSupplyTotal" (fun s -> s.pktRcvFilterSupplyTotal);
    int "pktRcvFilterLossTotal" (fun s -> s.pktRcvFilterLossTotal);
    int "pktSndFilterExtra" (fun s -> s.pktSndFilterExtra);
    int "pktRcvFilterExtra" (fun s -> s.pktRcvFilterExtra);
    int "pktRcvFilterSupply" (fun s -> s.pktRcvFilterSupply);
    int "pktRcvFilterLoss" (fun s -> s.pktRcvFilterLoss);
  ]

let stats_t =
  Lang.record_t (List.map (fun (name, typ, _) -> (name, typ)) stats_fields)

let stats_value stats =
  Lang.record (List.map (fun (name, _, get) -> (name, get stats)) stats_fields)

let nullable_bool p name =
  Lang.to_valued_option Lang.to_bool (List.assoc name p)

let other_methods =
  [
    ( "id",
      ([], Lang.int_t),
      "The libsrt socket id.",
      fun socket -> Lang.int (Srt.socket_id socket) );
    ( "status",
      ([], Lang.fun_t [] Lang.string_t),
      "Socket status.",
      fun socket ->
        Lang.val_fun [] (fun p ->
            srt_call p (fun () ->
                Lang.string (string_of_status (Srt.getsockstate socket)))) );
    ( "close",
      ([], Lang.fun_t [] Lang.unit_t),
      "Close the socket.",
      fun socket ->
        Lang.val_fun [] (fun p ->
            srt_call p (fun () -> Srt.close socket);
            Lang.unit) );
    ( "bstats",
      ([], Lang.fun_t [(true, "clear", Lang.nullable_t Lang.bool_t)] stats_t),
      "Socket statistics.",
      fun socket ->
        Lang.val_fun
          [("clear", "clear", Some Lang.null)]
          (fun p ->
            let clear = nullable_bool p "clear" in
            srt_call p (fun () -> stats_value (Srt.Stats.bstats ?clear socket)))
    );
    ( "bistats",
      ( [],
        Lang.fun_t
          [
            (true, "clear", Lang.nullable_t Lang.bool_t);
            (true, "instantaneous", Lang.nullable_t Lang.bool_t);
          ]
          stats_t ),
      "Socket statistics.",
      fun socket ->
        Lang.val_fun
          [
            ("clear", "clear", Some Lang.null);
            ("instantaneous", "instantaneous", Some Lang.null);
          ]
          (fun p ->
            let clear = nullable_bool p "clear" in
            let instantaneous = nullable_bool p "instantaneous" in
            srt_call p (fun () ->
                stats_value (Srt.Stats.bistats ?clear ?instantaneous socket)))
    );
  ]

let methods = option_methods @ other_methods

let t =
  Lang.method_t Base.t
    (List.map (fun (name, scheme, descr, _) -> (name, scheme, descr)) methods)

let to_value socket =
  Lang.meth (Base.to_value socket)
    (List.map (fun (name, _, _, value) -> (name, value socket)) methods)

let srt = Lang.add_module "srt"

let _ =
  Lang.add_builtin ~base:srt "socket" ~category:`Internet
    ~descr:"Add the SRT socket methods to an SRT socket value."
    [("", Base.t, None, None)]
    t
    (fun p -> to_value (Base.of_value (List.assoc "" p)))
