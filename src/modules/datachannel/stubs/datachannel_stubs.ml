open Ctypes

let const_string = typedef string "const char*"

module StateChangeCb =
  (val Foreign.dynamic_funptr ~thread_registration:true ~runtime_lock:true
         (int @-> int @-> ptr void @-> returning void))

module GatheringStateChangeCb =
  (val Foreign.dynamic_funptr ~thread_registration:true ~runtime_lock:true
         (int @-> int @-> ptr void @-> returning void))

module DescriptionCb =
  (val Foreign.dynamic_funptr ~thread_registration:true ~runtime_lock:true
         (int @-> ptr char @-> ptr char @-> ptr void @-> returning void))

module MessageCb =
  (val Foreign.dynamic_funptr ~thread_registration:true ~runtime_lock:true
         (int @-> ptr char @-> int @-> ptr void @-> returning void))

module Def (F : Cstubs.FOREIGN) = struct
  include Datachannel_types
  include Datachannel_types.Def (Datachannel_generated_types)
  open F

  let init_logger = foreign "rtcInitLogger" (int @-> ptr void @-> returning void)
  let cleanup = foreign "rtcCleanup" (void @-> returning void)

  let create_peer_connection =
    foreign "rtcCreatePeerConnection" (ptr RtcConfiguration.t @-> returning int)

  let delete_peer_connection =
    foreign "rtcDeletePeerConnection" (int @-> returning int)

  let set_user_pointer =
    foreign "rtcSetUserPointer" (int @-> ptr void @-> returning void)

  let set_state_change_callback =
    foreign "rtcSetStateChangeCallback"
      (int @-> StateChangeCb.t @-> returning int)

  let set_gathering_state_change_callback =
    foreign "rtcSetGatheringStateChangeCallback"
      (int @-> GatheringStateChangeCb.t @-> returning int)

  let set_local_description_callback =
    foreign "rtcSetLocalDescriptionCallback"
      (int @-> DescriptionCb.t @-> returning int)

  let add_track = foreign "rtcAddTrack" (int @-> const_string @-> returning int)

  let add_track_ex =
    foreign "rtcAddTrackEx" (int @-> ptr RtcTrackInit.t @-> returning int)

  let chain_rtcp_receiving_session =
    foreign "rtcChainRtcpReceivingSession" (int @-> returning int)

  let set_message_callback =
    foreign "rtcSetMessageCallback" (int @-> MessageCb.t @-> returning int)

  let set_local_description =
    foreign "rtcSetLocalDescription" (int @-> ptr char @-> returning int)

  let get_local_description =
    foreign "rtcGetLocalDescription" (int @-> ptr char @-> int @-> returning int)

  let get_local_description_type =
    foreign "rtcGetLocalDescriptionType"
      (int @-> ptr char @-> int @-> returning int)

  let set_remote_description =
    foreign "rtcSetRemoteDescription"
      (int @-> const_string @-> const_string @-> returning int)

  let close = foreign "rtcClose" (int @-> returning int)
  let delete = foreign "rtcDelete" (int @-> returning int)
end
