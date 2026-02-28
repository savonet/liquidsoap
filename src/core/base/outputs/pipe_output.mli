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

val output : Lang.module_name

val encoder_factory :
  ?format:Encoder.format ->
  Lang.value ->
  string ->
  Frame.Metadata.Export.t ->
  Encoder.encoder

val base_proto : Lang.proto

class virtual base :
  ?clock:Clock.t ->
  source:Lang.value ->
  name:string ->
  Lang.env ->
object
  val mutable current_metadata : Frame.Metadata.Export.t option
  val mutable encoder : [ `Created of Encoder.encoder | `None | `Pending ]
  method abort_track : unit
  method activations : Clock.activation list
  method active : bool
  method add_watcher : Source.watcher -> unit
  method private audio_channels : int
  method buffer : Generator.t
  method private can_generate_frame : bool
  method clear_last_metadata : unit
  method clock : Clock.t
  method close_encoder : Strings.t
  method consumed : int -> unit
  method content_type : Frame.content_type
  method content_type_computation_allowed : unit
  method create_encoder : unit
  method duration : int
  method effective_source : Source.source
  method elapsed : int
  method empty_frame : Frame.t
  method encode : Frame.t -> Strings.t
  method encode_metadata : Frame.Metadata.Export.t -> unit

  method private virtual encoder_factory :
    string -> Frame.Metadata.Export.t -> Encoder.encoder

  method end_of_track : Frame.t
  method private execute_on_track : Frame.t -> unit
  method execute_transition : Start_stop.state -> unit
  method fallible : bool
  method frame_audio_position : int
  method frame_has_track_mark : bool
  method frame_metadata : (int * Frame.Metadata.t) list
  method frame_position : int
  method frame_track_mark : int option
  method frame_type : Type.t
  method private generate_frame : Frame.t

  method private generate_video :
    ?create:
      (pos:int -> width:int -> height:int -> unit -> Mm.Video.Canvas.image) ->
    field:Frame.Fields.field ->
    int ->
    Content.Video.data

  method get_encoder : Encoder.encoder option
  method get_frame : Frame.t
  method get_mutable_content : Frame.field -> Content.data
  method get_mutable_frame : Frame.field -> Frame.t
  method get_partial_frame : (Frame.t -> Frame.t) -> Frame.t
  method has_content_type : bool
  method id : string
  method insert_metadata : new_track:bool -> Frame.metadata -> unit
  method is_ready : bool
  method is_up : bool
  method last_image : Frame.Fields.field -> Mm.Video.Canvas.image
  method last_metadata : (int * Frame.metadata) option
  method log : Log.t
  method private mutexify : 'a 'b. ('a -> 'b) -> 'a -> 'b

  method private nearest_image :
    pos:int ->
    last_image:Mm.Video.Canvas.image ->
    Content.Video.data ->
    Mm.Video.Canvas.image

  method on_after_streaming_cycle : (unit -> unit) -> unit
  method on_before_streaming_cycle : (unit -> unit) -> unit
  method on_frame : Source.on_frame -> unit
  method on_sleep : (unit -> unit) -> unit
  method on_start : (unit -> unit) -> unit
  method on_stop : (unit -> unit) -> unit
  method on_wake_up : (unit -> unit) -> unit
  method output : unit
  method peek_frame : Frame.t
  method pos : Pos.Option.t

  method register_command :
    ?usage:string -> descr:string -> string -> (string -> string) -> unit

  method remaining : int
  method reset : unit
  method reset_last_metadata_on_track : bool
  method private samplerate : float
  method seek : int -> int
  method virtual self_sync : Clock.self_sync
  method send : Strings.t -> unit
  method private send_frame : Frame.t -> unit

  method set_frame_data :
    Frame.field ->
    (?offset:int -> ?length:int -> 'a -> Content.data) ->
    'a ->
    Frame.t

  method set_id : ?force:bool -> string -> unit
  method private set_last_metadata : Frame.t -> unit
  method set_reset_last_metadata_on_track : bool -> unit
  method set_stack : Pos.t list -> unit
  method sleep : Clock.activation -> unit
  method source_sync : bool -> Clock.sync_source option
  method source_type : Source.source_type
  method private split_frame : Frame.t -> Frame.t * Frame.t option
  method stack : Pos.t list
  method stack_unifier : Pos.t list Unifier.t
  method virtual start : unit
  method state : Start_stop.state
  method virtual stop : unit
  method transition_to : Start_stop.state -> unit
  method private video_dimensions : int * int
  method wake_up : Clock.source -> Clock.activation
  method virtual write_pipe : string -> int -> int -> unit
end

val url_proto : Lang.t -> Lang.proto

val url_callbacks :
  < on_error : (bt:Printexc.raw_backtrace -> exn -> unit) -> unit ; .. >
  Lang.callback
  list

class url_output : Lang.env -> object
  val mutable current_metadata : Frame.Metadata.Export.t option
  val mutable encoder : [ `Created of Encoder.encoder | `None | `Pending ]
  val mutable on_error : (bt:Printexc.raw_backtrace -> exn -> unit) list
  val mutable on_start : (unit -> unit) list
  val mutable restart_time : float
  method abort_track : unit
  method activations : Clock.activation list
  method active : bool
  method add_watcher : Source.watcher -> unit
  method apply_on_error : bt:Printexc.raw_backtrace -> exn -> unit
  method private audio_channels : int
  method buffer : Generator.t
  method can_connect : bool
  method private can_generate_frame : bool
  method clear_last_metadata : unit
  method clock : Clock.t
  method close_encoder : Strings.t
  method connect : unit
  method consumed : int -> unit
  method content_type : Frame.content_type
  method content_type_computation_allowed : unit
  method create_encoder : unit
  method duration : int
  method effective_source : Source.source
  method elapsed : int
  method empty_frame : Frame.t
  method encode : Frame.t -> Strings.t
  method encode_metadata : Frame.Metadata.Export.t -> unit

  method private encoder_factory :
    string -> Frame.Metadata.Export.t -> Encoder.encoder

  method end_of_track : Frame.t
  method private execute_on_track : Frame.t -> unit
  method execute_transition : Start_stop.state -> unit
  method fallible : bool
  method frame_audio_position : int
  method frame_has_track_mark : bool
  method frame_metadata : (int * Frame.Metadata.t) list
  method frame_position : int
  method frame_track_mark : int option
  method frame_type : Type.t
  method private generate_frame : Frame.t

  method private generate_video :
    ?create:
      (pos:int -> width:int -> height:int -> unit -> Mm.Video.Canvas.image) ->
    field:Frame.Fields.field ->
    int ->
    Content.Video.data

  method get_encoder : Encoder.encoder option
  method get_frame : Frame.t
  method get_mutable_content : Frame.field -> Content.data
  method get_mutable_frame : Frame.field -> Frame.t
  method get_partial_frame : (Frame.t -> Frame.t) -> Frame.t
  method has_content_type : bool
  method id : string
  method insert_metadata : new_track:bool -> Frame.metadata -> unit
  method is_ready : bool
  method is_up : bool
  method last_image : Frame.Fields.field -> Mm.Video.Canvas.image
  method last_metadata : (int * Frame.metadata) option
  method log : Log.t
  method private mutexify : 'a 'b. ('a -> 'b) -> 'a -> 'b

  method private nearest_image :
    pos:int ->
    last_image:Mm.Video.Canvas.image ->
    Content.Video.data ->
    Mm.Video.Canvas.image

  method on_after_streaming_cycle : (unit -> unit) -> unit
  method on_before_streaming_cycle : (unit -> unit) -> unit
  method on_error : (bt:Printexc.raw_backtrace -> exn -> unit) -> unit
  method on_frame : Source.on_frame -> unit
  method on_sleep : (unit -> unit) -> unit
  method on_start : (unit -> unit) -> unit
  method on_stop : (unit -> unit) -> unit
  method on_wake_up : (unit -> unit) -> unit
  method output : unit
  method peek_frame : Frame.t
  method pos : Pos.Option.t

  method register_command :
    ?usage:string -> descr:string -> string -> (string -> string) -> unit

  method remaining : int
  method reset : unit
  method reset_last_metadata_on_track : bool
  method private samplerate : float
  method seek : int -> int
  method self_sync : Clock.self_sync
  method send : Strings.t -> unit
  method private send_frame : Frame.t -> unit

  method set_frame_data :
    Frame.field ->
    (?offset:int -> ?length:int -> 'a -> Content.data) ->
    'a ->
    Frame.t

  method set_id : ?force:bool -> string -> unit
  method private set_last_metadata : Frame.t -> unit
  method set_reset_last_metadata_on_track : bool -> unit
  method set_stack : Pos.t list -> unit
  method sleep : Clock.activation -> unit
  method source_sync : bool -> Clock.sync_source option
  method source_type : Source.source_type
  method private split_frame : Frame.t -> Frame.t * Frame.t option
  method stack : Pos.t list
  method stack_unifier : Pos.t list Unifier.t
  method start : unit
  method state : Start_stop.state
  method stop : unit
  method transition_to : Start_stop.state -> unit
  method private video_dimensions : int * int
  method wake_up : Clock.source -> Clock.activation
  method write_pipe : string -> int -> int -> unit
end

val default_reopen_on_error : Lang.value
val default_reopen_on_metadata : Lang.value
val default_reopen_when : Lang.value

val pipe_callbacks :
  < on_reopen : (unit -> unit) -> unit ; .. > Lang.callback list

val pipe_meth :
  (< abort_track : unit
   ; activations : Clock.activation list
   ; active : bool
   ; add_watcher : Source.watcher -> unit
   ; buffer : Generator.t
   ; cleanup_pipe : unit
   ; clear_last_metadata : unit
   ; clock : Clock.t
   ; close_encoder : Strings.t
   ; close_pipe : unit
   ; consumed : int -> unit
   ; content_type : Frame.content_type
   ; content_type_computation_allowed : unit
   ; create_encoder : unit
   ; duration : int
   ; effective_source : Source.source
   ; elapsed : int
   ; empty_frame : Frame.t
   ; encode : Frame.t -> Strings.t
   ; encode_metadata : Frame.Metadata.Export.t -> unit
   ; end_of_track : Frame.t
   ; execute_transition : Start_stop.state -> unit
   ; fallible : bool
   ; frame_audio_position : int
   ; frame_has_track_mark : bool
   ; frame_metadata : (int * Frame.Metadata.t) list
   ; frame_position : int
   ; frame_track_mark : int option
   ; frame_type : Type.t
   ; get_encoder : Encoder.encoder option
   ; get_frame : Frame.t
   ; get_mutable_content : Frame.field -> Content.data
   ; get_mutable_frame : Frame.field -> Frame.t
   ; get_partial_frame : (Frame.t -> Frame.t) -> Frame.t
   ; has_content_type : bool
   ; id : string
   ; insert_metadata : new_track:bool -> Frame.metadata -> unit
   ; interpolate : ?subst:(string -> string) -> string -> string
   ; is_open : bool
   ; is_ready : bool
   ; is_up : bool
   ; last_image : Frame.Fields.field -> Mm.Video.Canvas.image
   ; last_metadata : (int * Frame.metadata) option
   ; log : Log.t
   ; need_reopen : unit
   ; on_after_streaming_cycle : (unit -> unit) -> unit
   ; on_before_streaming_cycle : (unit -> unit) -> unit
   ; on_frame : Source.on_frame -> unit
   ; on_reopen : (unit -> unit) -> unit
   ; on_sleep : (unit -> unit) -> unit
   ; on_start : (unit -> unit) -> unit
   ; on_stop : (unit -> unit) -> unit
   ; on_wake_up : (unit -> unit) -> unit
   ; open_pipe : unit
   ; output : unit
   ; peek_frame : Frame.t
   ; pos : Pos.Option.t
   ; prepare_pipe : unit
   ; register_command :
       ?usage:string -> descr:string -> string -> (string -> string) -> unit
   ; remaining : int
   ; reopen : unit
   ; reset : unit
   ; reset_last_metadata_on_track : bool
   ; seek : int -> int
   ; self_sync : Clock.self_sync
   ; send : Strings.t -> unit
   ; set_frame_data :
       'a.
       Frame.field ->
       (?offset:int -> ?length:int -> 'a -> Content.data) ->
       'a ->
       Frame.t
   ; set_id : ?force:bool -> string -> unit
   ; set_reset_last_metadata_on_track : bool -> unit
   ; set_stack : Pos.t list -> unit
   ; sleep : Clock.activation -> unit
   ; source_sync : bool -> Clock.sync_source option
   ; source_type : Source.source_type
   ; stack : Pos.t list
   ; stack_unifier : Pos.t list Unifier.t
   ; start : unit
   ; state : Start_stop.state
   ; stop : unit
   ; transition_to : Start_stop.state -> unit
   ; wake_up : Clock.source -> Clock.activation
   ; write_pipe : string -> int -> int -> unit > ->
  Lang.value)
  Lang.meth
  list

class virtual piped_output : ?clock:Clock.t -> name:string -> Lang.env -> object
  val mutable current_metadata : Frame.Metadata.Export.t option
  val mutable encoder : [ `Created of Encoder.encoder | `None | `Pending ]
  val need_reopen : bool Atomic.t
  val mutable on_reopen : (unit -> unit) list
  val mutable open_date : float
  method abort_track : unit
  method activations : Clock.activation list
  method active : bool
  method add_watcher : Source.watcher -> unit
  method private audio_channels : int
  method buffer : Generator.t
  method private can_generate_frame : bool
  method cleanup_pipe : unit
  method clear_last_metadata : unit
  method clock : Clock.t
  method close_encoder : Strings.t
  method virtual close_pipe : unit
  method consumed : int -> unit
  method content_type : Frame.content_type
  method content_type_computation_allowed : unit
  method create_encoder : unit
  method duration : int
  method effective_source : Source.source
  method elapsed : int
  method empty_frame : Frame.t
  method encode : Frame.t -> Strings.t
  method encode_metadata : Frame.Metadata.Export.t -> unit

  method private virtual encoder_factory :
    string -> Frame.Metadata.Export.t -> Encoder.encoder

  method end_of_track : Frame.t
  method private execute_on_track : Frame.t -> unit
  method execute_transition : Start_stop.state -> unit
  method fallible : bool
  method frame_audio_position : int
  method frame_has_track_mark : bool
  method frame_metadata : (int * Frame.Metadata.t) list
  method frame_position : int
  method frame_track_mark : int option
  method frame_type : Type.t
  method private generate_frame : Frame.t

  method private generate_video :
    ?create:
      (pos:int -> width:int -> height:int -> unit -> Mm.Video.Canvas.image) ->
    field:Frame.Fields.field ->
    int ->
    Content.Video.data

  method get_encoder : Encoder.encoder option
  method get_frame : Frame.t
  method get_mutable_content : Frame.field -> Content.data
  method get_mutable_frame : Frame.field -> Frame.t
  method get_partial_frame : (Frame.t -> Frame.t) -> Frame.t
  method has_content_type : bool
  method id : string
  method insert_metadata : new_track:bool -> Frame.metadata -> unit
  method interpolate : ?subst:(string -> string) -> string -> string
  method virtual is_open : bool
  method is_ready : bool
  method is_up : bool
  method last_image : Frame.Fields.field -> Mm.Video.Canvas.image
  method last_metadata : (int * Frame.metadata) option
  method log : Log.t
  method private mutexify : 'a 'b. ('a -> 'b) -> 'a -> 'b

  method private nearest_image :
    pos:int ->
    last_image:Mm.Video.Canvas.image ->
    Content.Video.data ->
    Mm.Video.Canvas.image

  method need_reopen : unit
  method on_after_streaming_cycle : (unit -> unit) -> unit
  method on_before_streaming_cycle : (unit -> unit) -> unit
  method on_frame : Source.on_frame -> unit
  method on_reopen : (unit -> unit) -> unit
  method on_sleep : (unit -> unit) -> unit
  method on_start : (unit -> unit) -> unit
  method on_stop : (unit -> unit) -> unit
  method on_wake_up : (unit -> unit) -> unit
  method virtual open_pipe : unit
  method output : unit
  method peek_frame : Frame.t
  method pos : Pos.Option.t
  method prepare_pipe : unit

  method register_command :
    ?usage:string -> descr:string -> string -> (string -> string) -> unit

  method remaining : int
  method reopen : unit
  method private reopen_on_error : bt:Printexc.raw_backtrace -> exn -> unit
  method reset : unit
  method reset_last_metadata_on_track : bool
  method private samplerate : float
  method seek : int -> int
  method virtual self_sync : Clock.self_sync
  method send : Strings.t -> unit
  method private send_frame : Frame.t -> unit

  method set_frame_data :
    Frame.field ->
    (?offset:int -> ?length:int -> 'a -> Content.data) ->
    'a ->
    Frame.t

  method set_id : ?force:bool -> string -> unit
  method private set_last_metadata : Frame.t -> unit
  method set_reset_last_metadata_on_track : bool -> unit
  method set_stack : Pos.t list -> unit
  method sleep : Clock.activation -> unit
  method source_sync : bool -> Clock.sync_source option
  method source_type : Source.source_type
  method private split_frame : Frame.t -> Frame.t * Frame.t option
  method stack : Pos.t list
  method stack_unifier : Pos.t list Unifier.t
  method start : unit
  method state : Start_stop.state
  method stop : unit
  method transition_to : Start_stop.state -> unit
  method private video_dimensions : int * int
  method wake_up : Clock.source -> Clock.activation
  method virtual write_pipe : string -> int -> int -> unit
end

val chan_proto : Lang.t -> string -> Lang.proto

class virtual ['a] chan_output : Lang.env -> object
  val mutable chan : 'a option
  method virtual close_chan : 'a -> unit
  method close_pipe : unit
  method virtual flush : 'a -> unit
  method is_open : bool
  method virtual open_chan : 'a
  method open_pipe : unit
  method virtual output_substring : 'a -> string -> int -> int -> unit
  method write_pipe : string -> int -> int -> unit
end

class virtual ['a] file_output_base : Lang.env -> object
  val current_filename : string option Atomic.t
  method close_chan : 'a -> unit
  method virtual close_out : 'a -> unit
  method private filename : string
  method virtual interpolate : ?subst:(string -> string) -> string -> string
  method private on_close : string -> unit
  method open_chan : 'a
  method virtual open_out_gen : open_flag list -> int -> string -> 'a
  method private prepare_filename : string * open_flag list * int
  method self_sync : Clock.self_sync
end

class file_output :
  ?clock:Clock.t ->
  format_val:Lang.value ->
  Lang.env ->
object
  val mutable chan : out_channel option
  val current_filename : string option Atomic.t
  val mutable current_metadata : Frame.Metadata.Export.t option
  val mutable encoder : [ `Created of Encoder.encoder | `None | `Pending ]
  val need_reopen : bool Atomic.t
  val mutable on_reopen : (unit -> unit) list
  val mutable open_date : float
  method abort_track : unit
  method activations : Clock.activation list
  method active : bool
  method add_watcher : Source.watcher -> unit
  method private audio_channels : int
  method buffer : Generator.t
  method private can_generate_frame : bool
  method cleanup_pipe : unit
  method clear_last_metadata : unit
  method clock : Clock.t
  method close_chan : out_channel -> unit
  method close_encoder : Strings.t
  method close_out : out_channel -> unit
  method close_pipe : unit
  method consumed : int -> unit
  method content_type : Frame.content_type
  method content_type_computation_allowed : unit
  method create_encoder : unit
  method duration : int
  method effective_source : Source.source
  method elapsed : int
  method empty_frame : Frame.t
  method encode : Frame.t -> Strings.t
  method encode_metadata : Frame.Metadata.Export.t -> unit
  method encoder_factory : string -> Frame.Metadata.Export.t -> Encoder.encoder
  method end_of_track : Frame.t
  method private execute_on_track : Frame.t -> unit
  method execute_transition : Start_stop.state -> unit
  method fallible : bool
  method private filename : string
  method flush : out_channel -> unit
  method frame_audio_position : int
  method frame_has_track_mark : bool
  method frame_metadata : (int * Frame.Metadata.t) list
  method frame_position : int
  method frame_track_mark : int option
  method frame_type : Type.t
  method private generate_frame : Frame.t

  method private generate_video :
    ?create:
      (pos:int -> width:int -> height:int -> unit -> Mm.Video.Canvas.image) ->
    field:Frame.Fields.field ->
    int ->
    Content.Video.data

  method get_encoder : Encoder.encoder option
  method get_frame : Frame.t
  method get_mutable_content : Frame.field -> Content.data
  method get_mutable_frame : Frame.field -> Frame.t
  method get_partial_frame : (Frame.t -> Frame.t) -> Frame.t
  method has_content_type : bool
  method id : string
  method insert_metadata : new_track:bool -> Frame.metadata -> unit
  method interpolate : ?subst:(string -> string) -> string -> string
  method is_open : bool
  method is_ready : bool
  method is_up : bool
  method last_image : Frame.Fields.field -> Mm.Video.Canvas.image
  method last_metadata : (int * Frame.metadata) option
  method log : Log.t
  method private mutexify : 'a 'b. ('a -> 'b) -> 'a -> 'b

  method private nearest_image :
    pos:int ->
    last_image:Mm.Video.Canvas.image ->
    Content.Video.data ->
    Mm.Video.Canvas.image

  method need_reopen : unit
  method on_after_streaming_cycle : (unit -> unit) -> unit
  method on_before_streaming_cycle : (unit -> unit) -> unit
  method private on_close : string -> unit
  method on_frame : Source.on_frame -> unit
  method on_reopen : (unit -> unit) -> unit
  method on_sleep : (unit -> unit) -> unit
  method on_start : (unit -> unit) -> unit
  method on_stop : (unit -> unit) -> unit
  method on_wake_up : (unit -> unit) -> unit
  method open_chan : out_channel
  method open_out_gen : open_flag list -> int -> string -> out_channel
  method open_pipe : unit
  method output : unit
  method output_substring : out_channel -> string -> int -> int -> unit
  method peek_frame : Frame.t
  method pos : Pos.Option.t
  method private prepare_filename : string * open_flag list * int
  method prepare_pipe : unit

  method register_command :
    ?usage:string -> descr:string -> string -> (string -> string) -> unit

  method remaining : int
  method reopen : unit
  method private reopen_on_error : bt:Printexc.raw_backtrace -> exn -> unit
  method reset : unit
  method reset_last_metadata_on_track : bool
  method private samplerate : float
  method seek : int -> int
  method self_sync : Clock.self_sync
  method send : Strings.t -> unit
  method private send_frame : Frame.t -> unit

  method set_frame_data :
    Frame.field ->
    (?offset:int -> ?length:int -> 'a -> Content.data) ->
    'a ->
    Frame.t

  method set_id : ?force:bool -> string -> unit
  method private set_last_metadata : Frame.t -> unit
  method set_reset_last_metadata_on_track : bool -> unit
  method set_stack : Pos.t list -> unit
  method sleep : Clock.activation -> unit
  method source_sync : bool -> Clock.sync_source option
  method source_type : Source.source_type
  method private split_frame : Frame.t -> Frame.t * Frame.t option
  method stack : Pos.t list
  method stack_unifier : Pos.t list Unifier.t
  method start : unit
  method state : Start_stop.state
  method stop : unit
  method transition_to : Start_stop.state -> unit
  method private video_dimensions : int * int
  method wake_up : Clock.source -> Clock.activation
  method write_pipe : string -> int -> int -> unit
end

class file_output_using_encoder :
  ?clock:Clock.t ->
  format_val:Lang.value ->
  Lang.env ->
object
  val mutable chan : unit option
  val current_filename : string option Atomic.t
  val mutable current_metadata : Frame.Metadata.Export.t option
  val mutable encoder : [ `Created of Encoder.encoder | `None | `Pending ]
  val need_reopen : bool Atomic.t
  val mutable on_reopen : (unit -> unit) list
  val mutable open_date : float
  method abort_track : unit
  method activations : Clock.activation list
  method active : bool
  method add_watcher : Source.watcher -> unit
  method private audio_channels : int
  method buffer : Generator.t
  method private can_generate_frame : bool
  method cleanup_pipe : unit
  method clear_last_metadata : unit
  method clock : Clock.t
  method close_chan : unit -> unit
  method close_encoder : Strings.t
  method close_out : unit -> unit
  method close_pipe : unit
  method consumed : int -> unit
  method content_type : Frame.content_type
  method content_type_computation_allowed : unit
  method create_encoder : unit
  method duration : int
  method effective_source : Source.source
  method elapsed : int
  method empty_frame : Frame.t
  method encode : Frame.t -> Strings.t
  method encode_metadata : Frame.Metadata.Export.t -> unit
  method encoder_factory : string -> Frame.Metadata.Export.t -> Encoder.encoder
  method end_of_track : Frame.t
  method private execute_on_track : Frame.t -> unit
  method execute_transition : Start_stop.state -> unit
  method fallible : bool
  method private filename : string
  method flush : unit -> unit
  method frame_audio_position : int
  method frame_has_track_mark : bool
  method frame_metadata : (int * Frame.Metadata.t) list
  method frame_position : int
  method frame_track_mark : int option
  method frame_type : Type.t
  method private generate_frame : Frame.t

  method private generate_video :
    ?create:
      (pos:int -> width:int -> height:int -> unit -> Mm.Video.Canvas.image) ->
    field:Frame.Fields.field ->
    int ->
    Content.Video.data

  method get_encoder : Encoder.encoder option
  method get_frame : Frame.t
  method get_mutable_content : Frame.field -> Content.data
  method get_mutable_frame : Frame.field -> Frame.t
  method get_partial_frame : (Frame.t -> Frame.t) -> Frame.t
  method has_content_type : bool
  method id : string
  method insert_metadata : new_track:bool -> Frame.metadata -> unit
  method interpolate : ?subst:(string -> string) -> string -> string
  method is_open : bool
  method is_ready : bool
  method is_up : bool
  method last_image : Frame.Fields.field -> Mm.Video.Canvas.image
  method last_metadata : (int * Frame.metadata) option
  method log : Log.t
  method private mutexify : 'a 'b. ('a -> 'b) -> 'a -> 'b

  method private nearest_image :
    pos:int ->
    last_image:Mm.Video.Canvas.image ->
    Content.Video.data ->
    Mm.Video.Canvas.image

  method need_reopen : unit
  method on_after_streaming_cycle : (unit -> unit) -> unit
  method on_before_streaming_cycle : (unit -> unit) -> unit
  method private on_close : string -> unit
  method on_frame : Source.on_frame -> unit
  method on_reopen : (unit -> unit) -> unit
  method on_sleep : (unit -> unit) -> unit
  method on_start : (unit -> unit) -> unit
  method on_stop : (unit -> unit) -> unit
  method on_wake_up : (unit -> unit) -> unit
  method open_chan : unit
  method open_out_gen : open_flag list -> int -> string -> unit
  method open_pipe : unit
  method output : unit
  method output_substring : unit -> string -> int -> int -> unit
  method peek_frame : Frame.t
  method pos : Pos.Option.t
  method private prepare_filename : string * open_flag list * int
  method prepare_pipe : unit

  method register_command :
    ?usage:string -> descr:string -> string -> (string -> string) -> unit

  method remaining : int
  method reopen : unit
  method private reopen_on_error : bt:Printexc.raw_backtrace -> exn -> unit
  method reset : unit
  method reset_last_metadata_on_track : bool
  method private samplerate : float
  method seek : int -> int
  method self_sync : Clock.self_sync
  method send : Strings.t -> unit
  method private send_frame : Frame.t -> unit

  method set_frame_data :
    Frame.field ->
    (?offset:int -> ?length:int -> 'a -> Content.data) ->
    'a ->
    Frame.t

  method set_id : ?force:bool -> string -> unit
  method private set_last_metadata : Frame.t -> unit
  method set_reset_last_metadata_on_track : bool -> unit
  method set_stack : Pos.t list -> unit
  method sleep : Clock.activation -> unit
  method source_sync : bool -> Clock.sync_source option
  method source_type : Source.source_type
  method private split_frame : Frame.t -> Frame.t * Frame.t option
  method stack : Pos.t list
  method stack_unifier : Pos.t list Unifier.t
  method start : unit
  method state : Start_stop.state
  method stop : unit
  method transition_to : Start_stop.state -> unit
  method private video_dimensions : int * int
  method wake_up : Clock.source -> Clock.activation
  method write_pipe : string -> int -> int -> unit
end

val file_proto : Lang.t -> Lang.proto
val new_file_output : ?clock:Clock.t -> Lang.env -> piped_output
val output_file : Lang.module_name

class external_output : ?clock:Clock.t -> Lang.env -> object
  val mutable chan : out_channel option
  val mutable current_metadata : Frame.Metadata.Export.t option
  val mutable encoder : [ `Created of Encoder.encoder | `None | `Pending ]
  val need_reopen : bool Atomic.t
  val mutable on_reopen : (unit -> unit) list
  val mutable open_date : float
  method abort_track : unit
  method activations : Clock.activation list
  method active : bool
  method add_watcher : Source.watcher -> unit
  method private audio_channels : int
  method buffer : Generator.t
  method private can_generate_frame : bool
  method cleanup_pipe : unit
  method clear_last_metadata : unit
  method clock : Clock.t
  method close_chan : out_channel -> unit
  method close_encoder : Strings.t
  method close_out : out_channel -> unit
  method close_pipe : unit
  method consumed : int -> unit
  method content_type : Frame.content_type
  method content_type_computation_allowed : unit
  method create_encoder : unit
  method duration : int
  method effective_source : Source.source
  method elapsed : int
  method empty_frame : Frame.t
  method encode : Frame.t -> Strings.t
  method encode_metadata : Frame.Metadata.Export.t -> unit
  method encoder_factory : string -> Frame.Metadata.Export.t -> Encoder.encoder
  method end_of_track : Frame.t
  method private execute_on_track : Frame.t -> unit
  method execute_transition : Start_stop.state -> unit
  method fallible : bool
  method flush : out_channel -> unit
  method frame_audio_position : int
  method frame_has_track_mark : bool
  method frame_metadata : (int * Frame.Metadata.t) list
  method frame_position : int
  method frame_track_mark : int option
  method frame_type : Type.t
  method private generate_frame : Frame.t

  method private generate_video :
    ?create:
      (pos:int -> width:int -> height:int -> unit -> Mm.Video.Canvas.image) ->
    field:Frame.Fields.field ->
    int ->
    Content.Video.data

  method get_encoder : Encoder.encoder option
  method get_frame : Frame.t
  method get_mutable_content : Frame.field -> Content.data
  method get_mutable_frame : Frame.field -> Frame.t
  method get_partial_frame : (Frame.t -> Frame.t) -> Frame.t
  method has_content_type : bool
  method id : string
  method insert_metadata : new_track:bool -> Frame.metadata -> unit
  method interpolate : ?subst:(string -> string) -> string -> string
  method is_open : bool
  method is_ready : bool
  method is_up : bool
  method last_image : Frame.Fields.field -> Mm.Video.Canvas.image
  method last_metadata : (int * Frame.metadata) option
  method log : Log.t
  method private mutexify : 'a 'b. ('a -> 'b) -> 'a -> 'b

  method private nearest_image :
    pos:int ->
    last_image:Mm.Video.Canvas.image ->
    Content.Video.data ->
    Mm.Video.Canvas.image

  method need_reopen : unit
  method on_after_streaming_cycle : (unit -> unit) -> unit
  method on_before_streaming_cycle : (unit -> unit) -> unit
  method on_frame : Source.on_frame -> unit
  method on_reopen : (unit -> unit) -> unit
  method on_sleep : (unit -> unit) -> unit
  method on_start : (unit -> unit) -> unit
  method on_stop : (unit -> unit) -> unit
  method on_wake_up : (unit -> unit) -> unit
  method open_chan : out_channel
  method open_pipe : unit
  method output : unit
  method output_substring : out_channel -> string -> int -> int -> unit
  method peek_frame : Frame.t
  method pos : Pos.Option.t
  method prepare_pipe : unit

  method register_command :
    ?usage:string -> descr:string -> string -> (string -> string) -> unit

  method remaining : int
  method reopen : unit
  method private reopen_on_error : bt:Printexc.raw_backtrace -> exn -> unit
  method reset : unit
  method reset_last_metadata_on_track : bool
  method private samplerate : float
  method seek : int -> int
  method self_sync : Clock.self_sync
  method send : Strings.t -> unit
  method private send_frame : Frame.t -> unit

  method set_frame_data :
    Frame.field ->
    (?offset:int -> ?length:int -> 'a -> Content.data) ->
    'a ->
    Frame.t

  method set_id : ?force:bool -> string -> unit
  method private set_last_metadata : Frame.t -> unit
  method set_reset_last_metadata_on_track : bool -> unit
  method set_stack : Pos.t list -> unit
  method sleep : Clock.activation -> unit
  method source_sync : bool -> Clock.sync_source option
  method source_type : Source.source_type
  method private split_frame : Frame.t -> Frame.t * Frame.t option
  method stack : Pos.t list
  method stack_unifier : Pos.t list Unifier.t
  method start : unit
  method state : Start_stop.state
  method stop : unit
  method transition_to : Start_stop.state -> unit
  method private video_dimensions : int * int
  method wake_up : Clock.source -> Clock.activation
  method write_pipe : string -> int -> int -> unit
end

val pipe_proto : Lang.t -> string -> Lang.proto
