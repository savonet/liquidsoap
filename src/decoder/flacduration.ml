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

(** Read duration of FLAC files. *)

let duration file =
  let h = Flac.Decoder.File.create (fun _ -> ()) file in
  Tutils.finalize
    ~k:(fun () -> Unix.close h.Flac.Decoder.File.fd)
    (fun _ ->
      Int64.to_float h.Flac.Decoder.File.info.Flac.Decoder.total_samples
      /. float h.Flac.Decoder.File.info.Flac.Decoder.sample_rate)

let () = Request.dresolvers#register "FLAC" duration
