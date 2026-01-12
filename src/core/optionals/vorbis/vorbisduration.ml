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

(** Read duration of ogg/vorbis files. *)

let dresolver ~metadata:_ file =
  let dec, fd = Vorbis.File.Decoder.openfile file in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun _ -> Vorbis.File.Decoder.duration dec (-1))

let () =
  Plug.register Request.dresolvers "vorbis" ~doc:""
    {
      dpriority = (fun () -> Liq_ogg_decoder.priority#get);
      file_extensions = (fun () -> Liq_ogg_decoder.file_extensions#get);
      dresolver;
    }
