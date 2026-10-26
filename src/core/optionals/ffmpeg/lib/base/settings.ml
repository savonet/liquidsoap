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

let conf = Dtools.Conf.void ~p:(Configure.conf#plug "ffmpeg") "FFmpeg settings"

let log =
  Dtools.Conf.void ~p:(conf#plug "log") "Log settings"
    ~comments:["Settings for the bridge from FFmpeg's log to Liquidsoap's."]

let verbosities =
  [
    "quiet";
    "panic";
    "fatal";
    "error";
    "warning";
    "info";
    "verbose";
    "debug";
    "trace";
  ]

let log_verbosity =
  let key =
    Dtools.Conf.string ~p:(log#plug "verbosity") "Verbosity" ~d:"warning"
      ~comments:
        [
          "FFmpeg's own log level: messages less severe are discarded. One of: "
          ^ String.concat ", " (List.map (Printf.sprintf "`%S`") verbosities)
          ^ ".";
        ]
  in
  key#validate (fun v -> List.mem v verbosities);
  key

let log_level =
  Dtools.Conf.int ~p:(log#plug "level") "Level" ~d:3
    ~comments:
      [
        "The least severe Liquidsoap level a captured FFmpeg message is logged \
         at.";
      ]

let log_capture =
  Dtools.Conf.bool ~p:(log#plug "capture") "Capture" ~d:true
    ~comments:
      [
        "Send FFmpeg's messages to the Liquidsoap log; when false, FFmpeg \
         prints them to stderr.";
      ]

let autorotate =
  Dtools.Conf.bool ~p:(conf#plug "autorotate") "Autorotate" ~d:true
    ~comments:
      [
        "Rotate and flip decoded video by the display matrix of its stream, \
         and drop the matrix from the frames.";
      ]

let format_change =
  Dtools.Conf.void
    ~p:(conf#plug "format_change")
    "Format change settings"
    ~comments:["Which frame properties count as a change of video format."]

let format_change_color =
  Dtools.Conf.bool
    ~p:(format_change#plug "color")
    "Color" ~d:true
    ~comments:
      [
        "Colour space, range, primaries, transfer characteristic and chroma \
         location count as a format change.";
      ]

let format_change_pixel_aspect =
  Dtools.Conf.bool
    ~p:(format_change#plug "pixel_aspect")
    "Pixel aspect" ~d:true
    ~comments:["The pixel aspect ratio counts as a format change."]

let scaling_algorithms = ["fast_bilinear"; "bilinear"; "bicubic"]

let scaling_algorithm =
  let key =
    Dtools.Conf.string
      ~p:(conf#plug "scaling_algorithm")
      "Scaling algorithm" ~d:"bicubic"
      ~comments:
        [
          "Algorithm of every FFmpeg scaler. One of: "
          ^ String.concat ", "
              (List.map (Printf.sprintf "`%S`") scaling_algorithms)
          ^ ".";
        ]
  in
  key#validate (fun v -> List.mem v scaling_algorithms);
  key

let scaling_threads =
  Dtools.Conf.int
    ~p:(conf#plug "scaling_threads")
    "Scaling threads" ~d:1
    ~comments:
      [
        "Threads of each FFmpeg scaler: 1 scales on the calling thread, 0 uses \
         one thread per core.";
      ]

let filter_max_buffer =
  Dtools.Conf.float
    ~p:(conf#plug "filter_max_buffer")
    "Filter max buffer" ~d:10.
    ~comments:
      ["Maximum data buffered by one FFmpeg filter graph output, in seconds."]

let content =
  Dtools.Conf.void ~p:(conf#plug "content") "Content settings"
    ~comments:["Settings for FFmpeg content."]

let copy =
  Dtools.Conf.void ~p:(content#plug "copy") "Copy settings"
    ~comments:["Settings for copied FFmpeg content."]

let relaxed_compatibility_check =
  Dtools.Conf.bool
    ~p:(copy#plug "relaxed_compatibility_check")
    "Relaxed compatibility check" ~d:false
    ~comments:
      [
        "Advanced setting. Reduce the compatibility check of copied streams, \
         when a source is opened and when an output switches source, to the \
         media type and the codec. This lets sources of another codec form or \
         geometry into one copied stream: the muxer then decides whether the \
         output stays valid.";
      ]

let max_keyframe_wait =
  Dtools.Conf.float
    ~p:(copy#plug "max_keyframe_wait")
    "Max keyframe wait" ~d:20.
    ~comments:
      [
        "Longest wait, in seconds, of a copied source for the keyframes of its \
         streams before it starts without them.";
      ]
