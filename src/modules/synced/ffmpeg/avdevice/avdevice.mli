(** Tits module contains input and output devices for grabbing from and
    rendering to many common multimedia input/output software frameworks. *)

open Avutil

(** Initialize the module. This is done implicitly if you use any of the
    module's API but is here to provide an easy way to make sure that the module
    is explicitly linked by the OCaml compiler. NOT thread-safe! *)
val init : unit -> unit

(** Return the audio input devices formats. *)
val get_audio_input_formats : unit -> (input, audio) format list

(** Return the default audio input device format. *)
val get_default_audio_input_format : unit -> (input, audio) format

(** Return the video input devices formats. *)
val get_video_input_formats : unit -> (input, video) format list

(** Return the default video input device format. *)
val get_default_video_input_format : unit -> (input, video) format

(** Return the audio output devices formats. *)
val get_audio_output_formats : unit -> (output, audio) format list

(** Return the default audio output device format. *)
val get_default_audio_output_format : unit -> (output, audio) format

(** Return the video output devices formats. *)
val get_video_output_formats : unit -> (output, video) format list

(** Return the default video output device format. *)
val get_default_video_output_format : unit -> (output, video) format

(** Open the audio input device from its name. Raise Error if the device is not
    found. *)
val open_audio_input : string -> input container

(** Open the default audio input device from its name. Raise Error if the device
    is not found. *)
val open_default_audio_input : unit -> input container

(** Open the video input device from its name. Raise Error if the device is not
    found. *)
val open_video_input : string -> input container

(** Open the default video input device from its name. Raise Error if the device
    is not found. *)
val open_default_video_input : unit -> input container

(** Open the audio output device from its name. Raise Error if the device is not
    found. *)
val open_audio_output :
  ?interleaved:bool -> ?opts:opts -> string -> output container

(** Open the default audio output device from its name. Raise Error if the
    device is not found. *)
val open_default_audio_output :
  ?interleaved:bool -> ?opts:opts -> unit -> output container

(** Open the video output device from its name. Raise Error if the device is not
    found. *)
val open_video_output :
  ?interleaved:bool -> ?opts:opts -> string -> output container

(** Open the default video output device from its name. Raise Error if the
    device is not found. *)
val open_default_video_output :
  ?interleaved:bool -> ?opts:opts -> unit -> output container

(** Application to device communication *)
module App_to_dev : sig
  (** Application to device control messages *)
  type message =
    | None
    | Window_size of int * int * int * int
    | Window_repaint of int * int * int * int
    | Pause
    | Play
    | Toggle_pause
    | Set_volume of float
    | Mute
    | Unmute
    | Toggle_mute
    | Get_volume
    | Get_mute

  (** [Avdevice.App_to_dev.control_messages msg_list device] send the [msg_list]
      list of control message to the [device]. Raise Error if the application to
      device control message failed. *)
  val control_messages : message list -> _ container -> unit
end

(** Device to application communication *)
module Dev_to_app : sig
  (** Device to application control messages *)
  type message =
    | None
    | Create_window_buffer of (int * int * int * int) option
    | Prepare_window_buffer
    | Display_window_buffer
    | Destroy_window_buffer
    | Buffer_overflow
    | Buffer_underflow
    | Buffer_readable of Int64.t option
    | Buffer_writable of Int64.t option
    | Mute_state_changed of bool
    | Volume_level_changed of float

  (** [Avdevice.Dev_to_app.set_control_message_callback callback device] set the
      [callback] for [device] message reception. *)
  val set_control_message_callback : (message -> unit) -> _ container -> unit
end
