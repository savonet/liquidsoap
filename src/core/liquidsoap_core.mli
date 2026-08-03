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

(** The streaming engine.

    This module is the public face of [liquidsoap_core]: it re-exports, under
    short names, every module that operators, builtins and optional plugins are
    meant to use. Consumers get them unqualified through
    [-open Liquidsoap_core], which is why plugin code reads [Frame.] and [Lang.]
    rather than [Liquidsoap_core_stream.Frame.].

    A module that is not listed here is internal to the layer that defines it.
    Add it deliberately. *)

(** {1 Language}

    The liquidsoap language, re-exported so that operators do not have to spell
    out the wrapped `Liquidsoap_lang_*` paths. *)
module Lang = Lang

module Lang_source = Lang_source
module Lang_clock = Lang_clock
module Lang_encoder = Lang_encoder
module Lang_string = Lang_string
module Modules = Modules
module Doc = Doc
module Shebang = Shebang
module Type = Type
module Type_custom = Type_custom
module Typing = Typing
module Value = Value
module Term = Term
module Term_reducer = Term_reducer
module Parsed_term = Parsed_term
module Parser = Parser
module Preprocessor = Preprocessor
module Error = Error
module Runtime_error = Runtime_error
module Json = Json
module Methods = Methods
module Plug = Plug
module Pos = Pos
module Hooks = Hooks
module Cache = Cache
module Environment = Environment
module Profiler = Profiler
module Repr = Repr
module Flags = Flags

(** {1 Streams}

    Frames, the content they carry, and the types describing them. *)
module Frame = Frame

module Frame_base = Frame_base
module Frame_settings = Frame_settings
module Frame_type = Frame_type
module Format_type = Format_type
module AFrame = AFrame
module VFrame = VFrame
module MFrame = MFrame
module Content = Content
module Content_base = Content_base
module Content_audio = Content_audio
module Content_video = Content_video
module Content_pcm_s16 = Content_pcm_s16
module Content_pcm_f32 = Content_pcm_f32
module Subtitle_content = Subtitle_content
module Generator = Generator

(** {1 Sources and clocks}

    The streaming model: sources produce frames, clocks animate them. *)
module Source = Source

module Clock = Clock
module Output = Output
module Start_stop = Start_stop
module Child_support = Child_support
module Request = Request
module Request_dynamic = Request_dynamic
module Playlist_parser = Playlist_parser
module Annotate = Annotate

(** {1 Media}

    Decoding, encoding and format conversion. *)
module Decoder = Decoder

module Encoder = Encoder
module Encoder_utils = Encoder_utils
module Audio_converter = Audio_converter
module Video_converter = Video_converter
module Avi_format = Avi_format
module Fdkaac_format = Fdkaac_format
module Ffmpeg_format = Ffmpeg_format
module Flac_format = Flac_format
module Mp3_format = Mp3_format
module Ndi_format = Ndi_format
module Ogg_format = Ogg_format
module Opus_format = Opus_format
module Shine_format = Shine_format
module Speex_format = Speex_format
module Theora_format = Theora_format
module Vorbis_format = Vorbis_format

(** {1 Network}

    HTTP, the harbor server and the sources and outputs built on it. *)
module Harbor = Harbor

module Harbor_input = Harbor_input
module Pipe_output = Pipe_output
module Liq_http = Liq_http
module Liqcurl = Liqcurl
module Server = Server

(** {1 Operators}

    Operators that other operators build on. *)
module Amplify = Amplify

module Delay = Delay
module Filter = Filter
module Muxer = Muxer
module Keyboard = Keyboard
module Video_text = Video_text

(** {1 Utilities}

    Leaf utilities: logging, threads, processes, time, string handling. *)
module Configure = Configure

module Liquidsoap_paths = Liquidsoap_paths
module Startup = Startup
module Lifecycle = Lifecycle
module Log = Log
module Tutils = Tutils
module Process_handler = Process_handler
module Sandbox = Sandbox
module Liq_time = Liq_time
module Charset = Charset
module Utils = Utils
module Extralib = Extralib
module Strings = Strings
module Queues = Queues
module Mutex_utils = Mutex_utils
module Unifier = Unifier
module Extra_args = Extra_args
