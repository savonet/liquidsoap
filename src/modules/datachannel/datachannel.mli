(** Minimal OCaml bindings for libdatachannel's C API. Provides WebRTC peer
    connection, track, and media handling. *)

(** Peer connection state. *)
type state =
  [ `New | `Connecting | `Connected | `Disconnected | `Failed | `Closed ]

(** ICE gathering state. *)
type gathering_state = [ `New | `In_progress | `Complete ]

type log_level =
  | Log_none
  | Log_fatal
  | Log_error
  | Log_warning
  | Log_info
  | Log_debug
  | Log_verbose

(** Media track direction. *)
type direction =
  | Direction_unknown
  | Direction_sendonly
  | Direction_recvonly
  | Direction_sendrecv
  | Direction_inactive

(** Media codec. *)
type codec =
  | Codec_h264
  | Codec_vp8
  | Codec_vp9
  | Codec_h265
  | Codec_av1
  | Codec_opus
  | Codec_pcmu
  | Codec_pcma
  | Codec_aac

(** Track initialization parameters for {!add_track}. *)
type track_init = {
  direction : direction;
  codec : codec;
  payload_type : int;
  ssrc : int;
  mid : string option;
  name : string option;
  msid : string option;
  track_id : string option;
  profile : string option;
}

(** Initialize the libdatachannel logger at the given level. *)
val init_logger : log_level -> unit

(** Global cleanup. Call when done with all peer connections. *)
val cleanup : unit -> unit

(** Create a new peer connection with default configuration. Returns the peer
    connection identifier. *)
val create_peer_connection : unit -> int

(** Delete a peer connection. *)
val delete_peer_connection : int -> unit

(** Register a callback invoked when the peer connection state changes. *)
val set_state_change_callback : int -> (state -> unit) -> unit

(** Register a callback invoked when the ICE gathering state changes. *)
val set_gathering_state_change_callback :
  int -> (gathering_state -> unit) -> unit

(** Register a callback invoked when the local description (SDP) is available.
    The callback receives [(sdp, type)]. *)
val set_local_description_callback : int -> (string -> string -> unit) -> unit

(** Register a callback invoked when a message (e.g. an RTP packet) is received
    on a track or data channel. The callback receives [(data, size)]. *)
val set_message_callback : int -> (bytes -> int -> unit) -> unit

(** Add a media track from a raw SDP media description string. Returns the track
    identifier. *)
val add_track_sdp : int -> string -> int

(** Add a media track to the peer connection. Returns the track identifier. *)
val add_track : int -> track_init -> int

(** Chain an RTCP receiving session handler on a track. Required for receiving
    media. *)
val chain_rtcp_receiving_session : int -> unit

(** Trigger local description (SDP offer) generation. *)
val set_local_description : int -> unit

(** Retrieve the local SDP string after gathering is complete. *)
val get_local_description : int -> string

(** Retrieve the local SDP type string (e.g. "offer"). *)
val get_local_description_type : int -> string

(** Apply a remote SDP answer to the peer connection. *)
val set_remote_description : int -> sdp:string -> sdp_type:string -> unit

(** Close a track or data channel. *)
val close : int -> unit

(** Delete a track or data channel. *)
val delete : int -> unit
