(*
 * Copyright 2019 Savonet team
 *
 * This file is part of Ocaml-srt.
 *
 * Ocaml-srt is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-srt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-srt; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** OCaml bindings for the libsrt. *)

open Unsigned

type socket

type socket_status =
  [ `Init
  | `Opened
  | `Listening
  | `Connecting
  | `Connected
  | `Broken
  | `Closing
  | `Closed
  | `Nonexist ]

type transtype = [ `Live | `File | `Invalid ]
type ('a, 'b) socket_opt
type listen_callback = socket -> int -> Unix.sockaddr -> string option -> bool

(** Write only options. *)
val messageapi : ([ `Write ], bool) socket_opt

val payloadsize : ([ `Write ], int) socket_opt
val transtype : ([ `Write ], transtype) socket_opt
val conntimeo : ([ `Write ], int) socket_opt
val passphrase : ([ `Write ], string) socket_opt
val enforced_encryption : ([ `Write ], bool) socket_opt

(** Read only options. *)
val rcvdata : ([ `Read ], int) socket_opt

(** Read/write options. *)
val rcvsyn : ([ `Read | `Write ], bool) socket_opt

val sndsyn : ([ `Read | `Write ], bool) socket_opt
val rcvtimeo : ([ `Read | `Write ], int) socket_opt
val sndtimeo : ([ `Read | `Write ], int) socket_opt
val reuseaddr : ([ `Read | `Write ], bool) socket_opt
val rcvbuf : ([ `Read | `Write ], int) socket_opt
val sndbuf : ([ `Read | `Write ], int) socket_opt
val udp_rcvbuf : ([ `Read | `Write ], int) socket_opt
val udp_sndbuf : ([ `Read | `Write ], int) socket_opt
val rcvlatency : ([ `Read | `Write ], int) socket_opt
val streamid : ([ `Read | `Write ], string) socket_opt
val pbkeylen : ([ `Read | `Write ], int) socket_opt
val ipv6only : ([ `Read | `Write ], bool) socket_opt
val latency : ([ `Read | `Write ], int) socket_opt
val peerlatency : ([ `Read | `Write ], int) socket_opt

type errno =
  [ `Easyncfail
  | `Easyncrcv
  | `Easyncsnd
  | `Eboundsock
  | `Econgest
  | `Econnfail
  | `Econnlost
  | `Econnrej
  | `Econnsetup
  | `Econnsock
  | `Eduplisten
  | `Efile
  | `Einvalbufferapi
  | `Einvalmsgapi
  | `Einvop
  | `Einvparam
  | `Einvpollid
  | `Einvrdoff
  | `Einvsock
  | `Einvwroff
  | `Elargemsg
  | `Enobuf
  | `Enoconn
  | `Enolisten
  | `Enoserver
  | `Epeererr
  | `Epollempty
  | `Erdperm
  | `Erdvnoserv
  | `Erdvunbound
  | `Eresource
  | `Esclosed
  | `Esecfail
  | `Esockfail
  | `Esysobj
  | `Ethread
  | `Etimeout
  | `Eunboundsock
  | `Eunknown
  | `Ewrperm
  | `Success ]

exception Error of errno * string

val startup : unit -> unit
val cleanup : unit -> unit
val create_socket : unit -> socket
val socket_id : socket -> int
val getsockstate : socket -> socket_status
val bind : socket -> Unix.sockaddr -> unit
val bind_posix_socket : socket -> Posix_socket.sockaddr Ctypes.ptr -> unit
val listen_callback : socket -> listen_callback -> unit
val listen : socket -> int -> unit
val accept : socket -> socket * Unix.sockaddr
val accept_no_origin : socket -> socket
val connect : socket -> Unix.sockaddr -> unit
val connect_posix_socket : socket -> Posix_socket.sockaddr Ctypes.ptr -> unit

val rendez_vous_posix_socket :
  socket ->
  Posix_socket.sockaddr Ctypes.ptr ->
  Posix_socket.sockaddr Ctypes.ptr ->
  unit

val rendez_vous : socket -> Unix.sockaddr -> Unix.sockaddr -> unit
val send : socket -> bytes -> int
val recv : socket -> bytes -> int -> int
val sendmsg : socket -> bytes -> int -> bool -> int
val recvmsg : socket -> bytes -> int -> int
val getsockflag : socket -> ([> `Read ], 'a) socket_opt -> 'a
val setsockflag : socket -> ([> `Write ], 'a) socket_opt -> 'a -> unit
val close : socket -> unit

module Log : sig
  type msg = {
    level : int;
    file : string;
    line : int;
    area : string;
    message : string;
  }

  type level = [ `Critical | `Error | `Warning | `Notice | `Debug ]

  val setloglevel : level -> unit
  val set_handler : (msg -> unit) -> unit
  val clear_handler : unit -> unit
end

module Poll : sig
  type t
  type flag = [ `Read | `Write | `Error ]
  type event = { fd : socket; events : flag list }

  val create : unit -> t
  val sockets : t -> socket list
  val add_usock : ?flags:flag list -> t -> socket -> unit
  val remove_usock : t -> socket -> unit
  val update_usock : ?flags:flag list -> t -> socket -> unit
  val uwait : t -> timeout:int -> event list
  val wait : t -> timeout:int -> socket list * socket list
  val release : t -> unit
end

module Stats : sig
  type t = {
    msTimeStamp : int64;
    pktSentTotal : int64;
    pktRecvTotal : int64;
    pktSndLossTotal : int;
    pktRcvLossTotal : int;
    pktRetransTotal : int;
    pktSentACKTotal : int;
    pktRecvACKTotal : int;
    pktSentNAKTotal : int;
    pktRecvNAKTotal : int;
    usSndDurationTotal : int64;
    pktSndDropTotal : int;
    pktRcvDropTotal : int;
    pktRcvUndecryptTotal : int;
    byteSentTotal : UInt64.t;
    byteRecvTotal : UInt64.t;
    byteRetransTotal : UInt64.t;
    byteSndDropTotal : UInt64.t;
    byteRcvDropTotal : UInt64.t;
    byteRcvUndecryptTotal : UInt64.t;
    pktSent : int64;
    pktRecv : int64;
    pktSndLoss : int;
    pktRcvLoss : int;
    pktRetrans : int;
    pktRcvRetrans : int;
    pktSentACK : int;
    pktRecvACK : int;
    pktSentNAK : int;
    pktRecvNAK : int;
    mbpsSendRate : float;
    mbpsRecvRate : float;
    usSndDuration : int64;
    pktReorderDistance : int;
    pktRcvAvgBelatedTime : float;
    pktRcvBelated : int64;
    pktSndDrop : int;
    pktRcvDrop : int;
    pktRcvUndecrypt : int;
    byteSent : UInt64.t;
    byteRecv : UInt64.t;
    byteRetrans : UInt64.t;
    byteSndDrop : UInt64.t;
    byteRcvDrop : UInt64.t;
    byteRcvUndecrypt : UInt64.t;
    usPktSndPeriod : float;
    pktFlowWindow : int;
    pktCongestionWindow : int;
    pktFlightSize : int;
    msRTT : float;
    mbpsBandwidth : float;
    byteAvailSndBuf : int;
    byteAvailRcvBuf : int;
    mbpsMaxBW : float;
    byteMSS : int;
    pktSndBuf : int;
    byteSndBuf : int;
    msSndBuf : int;
    msSndTsbPdDelay : int;
    pktRcvBuf : int;
    byteRcvBuf : int;
    msRcvBuf : int;
    msRcvTsbPdDelay : int;
    pktSndFilterExtraTotal : int;
    pktRcvFilterExtraTotal : int;
    pktRcvFilterSupplyTotal : int;
    pktRcvFilterLossTotal : int;
    pktSndFilterExtra : int;
    pktRcvFilterExtra : int;
    pktRcvFilterSupply : int;
    pktRcvFilterLoss : int;
  }

  val bstats : ?clear:bool -> socket -> t
  val bistats : ?clear:bool -> ?instantaneous:bool -> socket -> t
end
