(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

(** NDI encoder. This encoder does nothing and can only by used with
    `output.ndi` *)

let encoder ~pos _ =
  let error () =
    Runtime_error.raise
      ~pos:(match pos with Some p -> [p] | None -> [])
      ~message:"The NDI encoder can only be used with `output.ndi`!" "invalid"
  in
  {
    Encoder.encode_metadata = (fun _ -> error ());
    header = error;
    hls = Encoder.dummy_hls (fun _ -> error ());
    encode = (fun _ -> error ());
    stop = error;
  }

let () =
  Plug.register Encoder.plug "ndi"
    ~doc:"NDI encoder. Only used with `output.ndi`." (function
    | Encoder.NDI m -> Some (fun ?hls:_ ~pos _ _ -> encoder ~pos m)
    | _ -> None)
