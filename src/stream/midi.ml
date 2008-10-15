(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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

type division =
  | Ticks_per_quarter of int
  | SMPTE of int * int

type event =
  | Note_off of int * int
  | Note_on of int * int
  | Aftertouch of int * int
  | Control_change of int * int
  | Patch of int
  | Channel_aftertouch of int
  | Pitch of int
  (* Meta-events *)
  | Sequence_number of int
  | Text of string
  | Copyright of string
  | Track_name of string
  | Instrument_name of string
  | Lyric of string
  | Marker of string
  | Cue of string
  | End_of_track
  | Tempo of int
  | Time_signature of int * int * int * int
  | Key_signature of int * bool
  | Custom of string

type header =
    {
      division : division
    }

type track = (int * event) list

let create_track () = []

(* TODO: !Fmt.ticks_per_second *)
let ticks_per_second = 44100

let ticks_of_delta division tempo delta =
  match division with
    | Ticks_per_quarter t ->
        (delta * 4 * 60 * ticks_per_second) / (t * tempo)
    | SMPTE (fps,res) ->
        (delta * ticks_per_second) / (fps * res)

let take ticks track = ()
