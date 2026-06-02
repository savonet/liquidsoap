(*
 Copyright (C) 2003-2006  Bardur Arantsson
 Copyright (C) 2006-2007  the Savonet Team

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Printf

let () =
  (* List all the drivers. *)
  List.iter
    (fun driver ->
      printf "\nid: %i\n" driver.Ao.id;
      printf "name: %s\n" driver.Ao.name;
      printf "short_name: %s\n" driver.Ao.short_name;
      printf "comment: %s\n" driver.Ao.comment;
      printf "author: %s\n" driver.Ao.author;
      printf "priority: %d\n" driver.Ao.priority;

      printf "pref. format: %s\n"
        (match driver.Ao.preferred_byte_format with
          | `NATIVE -> "native"
          | `BIG_ENDIAN -> "big endian"
          | `LITTLE_ENDIAN -> "little endian"
          | `UNKNOWN -> "(unknown)");

      printf "kind: %s\n"
        (match driver.Ao.kind with
          | `LIVE -> "Live"
          | `FILE -> "File"
          | `UNKNOWN -> "(unknown)");

      printf "options: [%s]\n" (String.concat "," driver.Ao.options))
    Ao.drivers;

  let device =
    Ao.open_live
      ~options:[("x", "y"); ("x1", "y1")]
      ~driver:(Ao.find_driver "null") ()
  in
  let buf = Bytes.create 41029 in
  (* Play garbage (literally!). *)
  Ao.play device (Bytes.to_string buf);
  (* Close device. *)
  Ao.close device;
  (* And we're done. *)
  printf "Goodbye!\n"
