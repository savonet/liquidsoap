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

(** Windows service runner. *)

let name = ref "Liquidsoap"
let display = ref "Liquidsoap Streaming Service"
let text = ref "Powerful streaming service using Liquidsoap"
let action = ref `None
let split s = Str.split (Str.regexp " ") s

module Runner : Main.Runner_t = struct
  let options =
    [
      ( ["--install-service"],
        Arg.Unit (fun _ -> action := `Install),
        "Install windows service running." );
      (["--service-name"], Arg.Set_string name, "Service name.");
      ( ["--service-title"],
        Arg.Set_string display,
        "Service title (displayed in service list)." );
      (["--service-description"], Arg.Set_string text, "Service description.");
      ( ["--remove-service"],
        Arg.Unit (fun _ -> action := `Remove),
        "Remove windows service." );
      ( ["--run-service"],
        Arg.Unit (fun _ -> action := `Run),
        "Run windows service (only used by windows service manager)." );
    ]
    @ Main.options
end

let () =
  let options = Main.expand_options Runner.options in
  Arg.parse options (fun _ -> ()) Main.usage;
  Arg.current := 0;
  let args =
    List.map
      (fun s -> if s <> "--install-service" then s else "--run-service")
      (List.tl (Array.to_list Sys.argv))
  in
  let module S = struct
    let name = !name
    let display = !display
    let text = !text
    let arguments = args
    let stop = Tutils.shutdown
  end in
  let module Svc = Service.Make (S) in
  let main () =
    let module Main = Main.Make (Runner) in
    ()
  in
  match !action with
    | `Install ->
        Svc.install ();
        Printf.printf "Installed %s service with arguments %s\n" S.name
          (String.concat " " args)
    | `Remove ->
        Svc.remove ();
        Printf.printf "Removed %s service\n" S.name
    | `Run -> (
        Dtools.Log.conf_stdout#set false;
        Dtools.Log.conf_file#set true;
        try Svc.run main
        with e ->
          Main.log#severe "Error while running service: %s"
            (Printexc.to_string e) )
    | `None -> main ()
