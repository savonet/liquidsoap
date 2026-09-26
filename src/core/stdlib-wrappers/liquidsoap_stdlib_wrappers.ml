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

module File = struct
  let[@inline] protect fn = Lang.protect ~kind:"file" fn
  let readdir dir = protect (fun () -> Sys.readdir dir)
  let digest file = protect (fun () -> Digest.file file)

  let temp_file ?temp_dir prefix suffix =
    protect (fun () -> Filename.temp_file ?temp_dir prefix suffix)

  let temp_dir prefix suffix =
    protect (fun () -> Filename.temp_dir prefix suffix)

  let openfile path flags perms =
    protect (fun () -> Unix.openfile path flags perms)

  (* FileUtil reports some failures through this callback instead of raising. *)
  let fileutil_error message _ = failwith message

  let fileutil_force force =
    if force then FileUtil.Force else FileUtil.Ask (fun _ -> false)

  let copy ~recurse ~force ~preserve src dst =
    protect (fun () ->
        FileUtil.cp ~recurse ~force:(fileutil_force force) ~preserve
          ~error:fileutil_error [src] dst)

  let move ~force src dst =
    protect (fun () ->
        FileUtil.mv ~force:(fileutil_force force) ~error:fileutil_error src dst)

  let rename src dst =
    protect (fun () ->
        try Unix.rename src dst
        with Unix.Unix_error (Unix.EXDEV, _, _) ->
          Lang.raise_error ~pos:[]
            ~message:
              "Rename failed! Directory for temporary files appears to be on a \
               different filesystem"
            "file.cross_device")
end

module Socket = struct
  let[@inline] protect fn = Lang.protect ~kind:"socket" fn

  let socket ?cloexec domain typ protocol =
    protect (fun () -> Unix.socket ?cloexec domain typ protocol)

  let socketpair ?cloexec domain typ protocol =
    protect (fun () -> Unix.socketpair ?cloexec domain typ protocol)

  let bind fd addr = protect (fun () -> Unix.bind fd addr)
  let listen fd max = protect (fun () -> Unix.listen fd max)
  let connect fd addr = protect (fun () -> Unix_utils.connect fd addr)
end
