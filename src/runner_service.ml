(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2013 Savonet team

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

(** Windows service runner. *)

module S = struct
  let name    = "Liquidsoap"
  let display = "Liquidsoap streaming service"
  let text    = "Powerful and flexible streaming language"
  let stop    = ref false
end

module Svc = Service.Make(S)

module Runner =
struct
  let usage =
    "Usage : liquidsoap.exe --install-service | --remove-service\n\
    \        liquidsoap.exe [OPTION, SCRIPT or EXPR]...\n\
    \ - SCRIPT for evaluating a liquidsoap script file;\n\
    \ - EXPR for evaluating a scripting expression;\n\
    \ - OPTION is one of the options listed below:\n"

  let stop () = !S.stop

  let options = [
        ["--install-service"],
        Arg.Unit (fun _ -> ()),
        "Install windows service." ;
        ["--remove-service"],
        Arg.Unit (fun _ -> ()),
        "Remove windows service." ;
  ]
end

let () =
  match List.tl (Array.to_list Sys.argv) with
    | ["--install-service"] ->
        Svc.install ();
        Printf.printf "Installed %s service\n" S.name
    | ["--remove-service"] ->
        Svc.remove ();
        Printf.printf "Removed %s service\n" S.name
    | _ ->
      begin
       let log = ref (fun _ -> ()) in
       try
        Svc.run (fun () ->
          let module Runner =
            Main.Run(Runner)
          in
          log := (Runner.log#f 2 "%s"))
       with
         | e ->
             !log (Printf.sprintf "Error while running service: %s"
                        (Utils.error_message e))
      end
