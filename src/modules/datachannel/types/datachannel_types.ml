module Constants = Datachannel_constants.Def (Datachannel_generated_constants)
include Constants

module Def (S : Cstubs.Types.TYPE) = struct
  module RtcConfiguration = struct
    type t = unit

    let t : t Ctypes.structure S.typ = S.structure "rtcConfiguration"
    let t = S.typedef t "rtcConfiguration"
    let ice_servers = S.field t "iceServers" S.(ptr (ptr char))
    let ice_servers_count = S.field t "iceServersCount" S.int
    let proxy_server = S.field t "proxyServer" S.(ptr char)
    let bind_address = S.field t "bindAddress" S.(ptr char)
    let certificate_type = S.field t "certificateType" S.int
    let ice_transport_policy = S.field t "iceTransportPolicy" S.int
    let enable_ice_tcp = S.field t "enableIceTcp" S.bool
    let enable_ice_udp_mux = S.field t "enableIceUdpMux" S.bool
    let disable_auto_negotiation = S.field t "disableAutoNegotiation" S.bool
    let force_media_transport = S.field t "forceMediaTransport" S.bool
    let port_range_begin = S.field t "portRangeBegin" S.uint16_t
    let port_range_end = S.field t "portRangeEnd" S.uint16_t
    let mtu = S.field t "mtu" S.int
    let max_message_size = S.field t "maxMessageSize" S.int
    let () = S.seal t
  end

  module RtcTrackInit = struct
    type t = unit

    let t : t Ctypes.structure S.typ = S.structure "rtcTrackInit"
    let t = S.typedef t "rtcTrackInit"
    let direction = S.field t "direction" S.int
    let codec = S.field t "codec" S.int
    let payload_type = S.field t "payloadType" S.int
    let ssrc = S.field t "ssrc" S.uint32_t
    let mid = S.field t "mid" S.(ptr char)
    let name = S.field t "name" S.(ptr char)
    let msid = S.field t "msid" S.(ptr char)
    let track_id = S.field t "trackId" S.(ptr char)
    let profile = S.field t "profile" S.(ptr char)
    let () = S.seal t
  end
end
