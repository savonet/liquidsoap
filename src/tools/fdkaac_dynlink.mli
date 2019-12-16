module type Fdkaac_t = sig
  module Encoder : sig
    exception Invalid_handle
    exception Unsupported_parameter
    exception Invalid_config
    exception Error of int
    exception End_of_file
    exception Unknown of int

    val string_of_exception : exn -> string option

    type t
    type mpeg2_aac = [ `AAC_LC | `HE_AAC | `HE_AAC_v2 ]
    type mpeg4_aac = [ `AAC_ELD | `AAC_LC | `AAC_LD | `HE_AAC | `HE_AAC_v2 ]
    type aot = [ `Mpeg_2 of mpeg2_aac | `Mpeg_4 of mpeg4_aac ]
    type bitrate_mode = [ `Constant | `Variable of int | `Full_bitreservoir ]
    type transmux = [ `Adif | `Adts | `Latm | `Latm_out_of_band | `Loas | `Raw ]

    type param_name =
      [ `Afterburner
      | `Aot
      | `Bandwidth
      | `Bitrate
      | `Bitrate_mode
      | `Granule_length
      | `Samplerate
      | `Sbr_mode
      | `Transmux ]

    type param =
      [ `Afterburner of bool
      | `Aot of aot
      | `Bandwidth of int
      | `Bitrate of int
      | `Bitrate_mode of bitrate_mode
      | `Granule_length of int
      | `Samplerate of int
      | `Sbr_mode of bool
      | `Transmux of transmux ]

    val create : int -> t
    val set : t -> param -> unit
    val get : t -> param_name -> param
    val encode : t -> string -> int -> int -> string
    val flush : t -> string
  end
end

type handler = { mutable fdkaac_module : (module Fdkaac_t) option }

val handler : handler
