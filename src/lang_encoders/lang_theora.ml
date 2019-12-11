(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

open Lang_values
open Lang_encoders

let make params =
  let defaults =
    {
      Theora_format.bitrate_control= Theora_format.Quality 40;
      fill= None;
      width= Frame.video_width;
      height= Frame.video_height;
      picture_width= Frame.video_width;
      picture_height= Frame.video_height;
      picture_x= 0;
      picture_y= 0;
      aspect_numerator= 1;
      aspect_denominator= 1;
      keyframe_frequency= 64;
      vp3_compatible= None;
      soft_target= false;
      buffer_delay= None;
      speed= None;
    }
  in
  let theora =
    List.fold_left
      (fun f -> function "quality", ({term= Int i; _} as t) ->
            (* According to the doc, this should be a value between
             * 0 and 63. *)
            if i < 0 || i > 63 then
              raise (Error (t, "Theora quality should be in 0..63")) ;
            {f with Theora_format.bitrate_control= Theora_format.Quality i}
        | "bitrate", {term= Int i; _} ->
            {f with Theora_format.bitrate_control= Theora_format.Bitrate i}
        | "width", ({term= Int i; _} as t) ->
            (* According to the doc: must be a multiple of 16, and less than 1048576. *)
            if i mod 16 <> 0 || i >= 1048576 then
              raise
                (Error
                   (t, "invalid frame width value (should be a multiple of 16)")) ;
            {f with Theora_format.width= lazy i; picture_width= lazy i}
        | "height", ({term= Int i; _} as t) ->
            (* According to the doc: must be a multiple of 16, and less than 1048576. *)
            if i mod 16 <> 0 || i >= 1048576 then
              raise
                (Error
                   ( t,
                     "invalid frame height value (should be a multiple of 16)"
                   )) ;
            {f with Theora_format.height= lazy i; picture_height= lazy i}
        | "picture_width", ({term= Int i; _} as t) ->
            (* According to the doc: must not be larger than width. *)
            if i > Lazy.force f.Theora_format.width then
              raise (Error (t, "picture width must not be larger than width")) ;
            {f with Theora_format.picture_width= lazy i}
        | "picture_height", ({term= Int i; _} as t) ->
            (* According to the doc: must not be larger than height. *)
            if i > Lazy.force f.Theora_format.height then
              raise
                (Error (t, "picture height must not be larger than height")) ;
            {f with Theora_format.picture_height= lazy i}
        | "picture_x", ({term= Int i; _} as t) ->
            (* According to the doc: must be no larger than width-picture_width
             * or 255, whichever is smaller. *)
            if
              i
              > min
                  ( Lazy.force f.Theora_format.width
                  - Lazy.force f.Theora_format.picture_width )
                  255
            then
              raise
                (Error
                   ( t,
                     "picture x must not be larger than width - picture width \
                      or 255, whichever is smaller" )) ;
            {f with Theora_format.picture_x= i}
        | "picture_y", ({term= Int i; _} as t) ->
            (* According to the doc: must be no larger than width-picture_width
             * and frame_height-pic_height-pic_y must be no larger than 255. *)
            if
              i
              > Lazy.force f.Theora_format.height
                - Lazy.force f.Theora_format.picture_height
            then
              raise
                (Error
                   ( t,
                     "picture y must not be larger than height - picture height"
                   )) ;
            if Lazy.force f.Theora_format.picture_height - i > 255 then
              raise
                (Error
                   (t, "picture height - picture y must not be larger than 255")) ;
            {f with Theora_format.picture_y= i}
        | "aspect_numerator", {term= Int i; _} ->
            {f with Theora_format.aspect_numerator= i}
        | "aspect_denominator", {term= Int i; _} ->
            {f with Theora_format.aspect_denominator= i}
        | "keyframe_frequency", {term= Int i; _} ->
            {f with Theora_format.keyframe_frequency= i}
        | "vp3_compatible", {term= Bool i; _} ->
            {f with Theora_format.vp3_compatible= Some i}
        | "soft_target", {term= Bool i; _} ->
            {f with Theora_format.soft_target= i}
        | "buffer_delay", {term= Int i; _} ->
            {f with Theora_format.buffer_delay= Some i}
        | "speed", {term= Int i; _} -> {f with Theora_format.speed= Some i}
        | "bytes_per_page", {term= Int i; _} ->
            {f with Theora_format.fill= Some i} | _, t ->
            raise (generic_error t))
      defaults params
  in
  Ogg_format.Theora theora
