open Ctypes
module C = Datachannel_stubs.Def (Datachannel_generated_stubs)

type state =
  [ `New | `Connecting | `Connected | `Disconnected | `Failed | `Closed ]

type gathering_state = [ `New | `In_progress | `Complete ]

type log_level =
  | Log_none
  | Log_fatal
  | Log_error
  | Log_warning
  | Log_info
  | Log_debug
  | Log_verbose

type direction =
  | Direction_unknown
  | Direction_sendonly
  | Direction_recvonly
  | Direction_sendrecv
  | Direction_inactive

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

let int_of_log_level = function
  | Log_none -> Int64.to_int C.rtc_log_none
  | Log_fatal -> Int64.to_int C.rtc_log_fatal
  | Log_error -> Int64.to_int C.rtc_log_error
  | Log_warning -> Int64.to_int C.rtc_log_warning
  | Log_info -> Int64.to_int C.rtc_log_info
  | Log_debug -> Int64.to_int C.rtc_log_debug
  | Log_verbose -> Int64.to_int C.rtc_log_verbose

let int_of_direction = function
  | Direction_unknown -> Int64.to_int C.rtc_direction_unknown
  | Direction_sendonly -> Int64.to_int C.rtc_direction_sendonly
  | Direction_recvonly -> Int64.to_int C.rtc_direction_recvonly
  | Direction_sendrecv -> Int64.to_int C.rtc_direction_sendrecv
  | Direction_inactive -> Int64.to_int C.rtc_direction_inactive

let int_of_codec = function
  | Codec_h264 -> Int64.to_int C.rtc_codec_h264
  | Codec_vp8 -> Int64.to_int C.rtc_codec_vp8
  | Codec_vp9 -> Int64.to_int C.rtc_codec_vp9
  | Codec_h265 -> Int64.to_int C.rtc_codec_h265
  | Codec_av1 -> Int64.to_int C.rtc_codec_av1
  | Codec_opus -> Int64.to_int C.rtc_codec_opus
  | Codec_pcmu -> Int64.to_int C.rtc_codec_pcmu
  | Codec_pcma -> Int64.to_int C.rtc_codec_pcma
  | Codec_aac -> Int64.to_int C.rtc_codec_aac

let state_of_int v =
  let v = Int64.of_int v in
  if v = C.rtc_new then `New
  else if v = C.rtc_connecting then `Connecting
  else if v = C.rtc_connected then `Connected
  else if v = C.rtc_disconnected then `Disconnected
  else if v = C.rtc_failed then `Failed
  else if v = C.rtc_closed then `Closed
  else failwith (Printf.sprintf "datachannel: unknown state %Ld" v)

let gathering_state_of_int v =
  let v = Int64.of_int v in
  if v = C.rtc_gathering_new then `New
  else if v = C.rtc_gathering_inprogress then `In_progress
  else if v = C.rtc_gathering_complete then `Complete
  else failwith (Printf.sprintf "datachannel: unknown gathering state %Ld" v)

let init_logger level = C.init_logger (int_of_log_level level) null
let cleanup () = C.cleanup ()

let check_error label ret =
  if ret < 0 then
    failwith (Printf.sprintf "datachannel: %s failed (%d)" label ret)

let create_peer_connection () =
  let config = allocate_n C.RtcConfiguration.t ~count:1 in
  let pc = C.create_peer_connection config in
  if pc < 0 then failwith "datachannel: create_peer_connection failed";
  pc

let delete_peer_connection pc =
  check_error "delete_peer_connection" (C.delete_peer_connection pc)

(* Dynamic function pointers must be rooted to prevent GC collection *)
let callback_roots : (int, Obj.t list) Hashtbl.t = Hashtbl.create 16

let add_root id obj =
  let existing = try Hashtbl.find callback_roots id with Not_found -> [] in
  Hashtbl.replace callback_roots id (obj :: existing)

let clear_roots id = Hashtbl.remove callback_roots id

let set_state_change_callback pc f =
  let cb _pc state _ptr = f (state_of_int state) in
  let fptr = Datachannel_stubs.StateChangeCb.of_fun cb in
  add_root pc (Obj.repr fptr);
  check_error "set_state_change_callback" (C.set_state_change_callback pc fptr)

let set_gathering_state_change_callback pc f =
  let cb _pc state _ptr = f (gathering_state_of_int state) in
  let fptr = Datachannel_stubs.GatheringStateChangeCb.of_fun cb in
  add_root pc (Obj.repr fptr);
  check_error "set_gathering_state_change_callback"
    (C.set_gathering_state_change_callback pc fptr)

let set_local_description_callback pc f =
  let cb _pc sdp sdp_type _ptr =
    let sdp = coerce (ptr char) string sdp in
    let sdp_type = coerce (ptr char) string sdp_type in
    f sdp sdp_type
  in
  let fptr = Datachannel_stubs.DescriptionCb.of_fun cb in
  add_root pc (Obj.repr fptr);
  check_error "set_local_description_callback"
    (C.set_local_description_callback pc fptr)

let set_message_callback id f =
  let cb _id data size _ptr =
    let buf = Bytes.create size in
    for i = 0 to size - 1 do
      Bytes.unsafe_set buf i !@(data +@ i)
    done;
    f buf size
  in
  let fptr = Datachannel_stubs.MessageCb.of_fun cb in
  add_root id (Obj.repr fptr);
  check_error "set_message_callback" (C.set_message_callback id fptr)

let add_track_sdp pc sdp =
  let tr = C.add_track pc sdp in
  if tr < 0 then failwith "datachannel: add_track_sdp failed";
  tr

let add_track pc init =
  let ti = allocate_n C.RtcTrackInit.t ~count:1 in
  ti |-> C.RtcTrackInit.direction <-@ int_of_direction init.direction;
  ti |-> C.RtcTrackInit.codec <-@ int_of_codec init.codec;
  ti |-> C.RtcTrackInit.payload_type <-@ init.payload_type;
  ti |-> C.RtcTrackInit.ssrc <-@ Unsigned.UInt32.of_int init.ssrc;
  let tr = C.add_track_ex pc ti in
  if tr < 0 then failwith "datachannel: add_track failed";
  tr

let chain_rtcp_receiving_session tr =
  check_error "chain_rtcp_receiving_session" (C.chain_rtcp_receiving_session tr)

let set_local_description pc =
  check_error "set_local_description"
    (C.set_local_description pc (from_voidp char null))

(* rtcGet*Description returns length including null terminator *)
let get_local_description pc =
  let buf_size = 16384 in
  let buf = CArray.make char buf_size in
  let ret = C.get_local_description pc (CArray.start buf) buf_size in
  if ret < 0 then failwith "datachannel: get_local_description failed";
  let len = max 0 (min ret buf_size - 1) in
  String.init len (fun i -> CArray.get buf i)

let get_local_description_type pc =
  let buf_size = 256 in
  let buf = CArray.make char buf_size in
  let ret = C.get_local_description_type pc (CArray.start buf) buf_size in
  if ret < 0 then failwith "datachannel: get_local_description_type failed";
  let len = max 0 (min ret buf_size - 1) in
  String.init len (fun i -> CArray.get buf i)

let set_remote_description pc ~sdp ~sdp_type =
  check_error "set_remote_description"
    (C.set_remote_description pc sdp sdp_type)

let close id =
  clear_roots id;
  check_error "close" (C.close id)

let delete id =
  clear_roots id;
  check_error "delete" (C.delete id)
