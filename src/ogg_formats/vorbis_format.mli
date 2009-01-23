(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(* Vorbis decoder does not need to be exposed.. *)

(* Vorbis encoder *)

val create_abr : channels:int -> samplerate:int -> 
                 min_rate:int -> max_rate:int -> 
                 average_rate:int -> metadata:((string*string) list) -> 
                 unit -> Ogg_encoder.stream_encoder 

val create_cbr : channels:int -> samplerate:int -> 
                 bitrate:int -> metadata:((string*string) list) -> 
                 unit -> Ogg_encoder.stream_encoder

val create : channels:int -> samplerate:int -> 
             quality:float -> metadata:((string*string) list) -> 
             unit -> Ogg_encoder.stream_encoder 

