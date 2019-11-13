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

let log = Log.make ["sandbox"]

let conf_sandbox =
  Dtools.Conf.string
    ~p:(Configure.conf#plug "sandbox")
    ~d:"disabled"
    "Use sandboxing for external process. One of: `\"enabled\"`, \
     `\"disabled\"` or `\"auto\"`."

let tmpdir = Filename.get_temp_dir_name ()

let conf_setenv =
  let f = Printf.sprintf "%s=%s" in
  let default_env =
    [f "TEMPDIR" tmpdir; f "TEMP" tmpdir; f "TMPDIR" tmpdir; f "TMP" tmpdir]
  in
  Dtools.Conf.list
    ~p:(conf_sandbox#plug "setenv")
    ~d:default_env "Additional default environment variables."

let get_setenv () =
  List.fold_left
    (fun cur s ->
      match Pcre.split ~pat:"=" s with
        | [] ->
            cur
        | lbl :: l ->
            (lbl, String.concat "=" l) :: cur)
    [] conf_setenv#get

let conf_unsetenv =
  Dtools.Conf.list
    ~p:(conf_sandbox#plug "unsetenv")
    ~d:[] "Environment varialbes to unset."

let conf_binary =
  Dtools.Conf.string
    ~p:(conf_sandbox#plug "binary")
    ~d:"bwrap" "Sandbox binary to use."

let conf_rw =
  let rw = [tmpdir] in
  let rw = match Sys.getenv_opt "HOME" with Some h -> h :: rw | None -> rw in
  Dtools.Conf.list ~p:(conf_sandbox#plug "rw") ~d:rw
    "Read/write directories. Default: `[$HOME;$TMPDIR]`."

let conf_ro =
  Dtools.Conf.list ~p:(conf_sandbox#plug "ro") ~d:["/"] "Read-only directories"

let conf_network =
  Dtools.Conf.bool ~p:(conf_sandbox#plug "network") ~d:true "Enable network"

let conf_shell =
  Dtools.Conf.bool
    ~p:(conf_sandbox#plug "shell")
    ~d:true "Run command inside shell."

let conf_shell_path =
  let d =
    match Sys.getenv_opt "SHELL" with Some shell -> shell | None -> "/bin/sh"
  in
  Dtools.Conf.string ~p:(conf_shell#plug "path") ~d
    "Patch to shell binary. Defaults to `$SHELL` if set and \"/bin/sh\" \
     otherwise."

let is_docker =
  lazy
    ( Sys.unix
    && Sys.command "grep 'docker\\|lxc' /proc/1/cgroup >/dev/null 2>&1" = 0 )

let has_binary =
  lazy (Utils.which_opt ~path:Configure.path conf_binary#get <> None)

let () =
  ignore
    (Dtools.Init.at_start (fun () ->
         if Lazy.force is_docker then (
           log#important
             "Running inside a docker container, disabling sandboxing.." ;
           conf_sandbox#set "disabled" )
         else if not (Lazy.force has_binary) then (
           log#important "Could not find binary %s, disabling sandboxing.."
             conf_binary#get ;
           conf_sandbox#set "disabled" )
         else if conf_sandbox#get = "disabled" then
           log#important "Sandboxing disabled"
         else (
           log#important "Sandboxing external processes using bubblewrap at %s"
             (Utils.which ~path:Configure.path conf_binary#get) ;
           log#important "Set environment variables: %s"
             (String.concat ", "
                (List.map
                   (fun (lbl, v) -> Printf.sprintf "%s=%S" lbl v)
                   (get_setenv ()))) ;
           log#important "Unset environment variables: %s"
             (String.concat ", " conf_unsetenv#get) ;
           log#important "Network allowed: %b" conf_network#get ;
           log#important "Read-only directories: %s"
             (String.concat ", " conf_ro#get) ;
           log#important "Read/write directories: %s"
             (String.concat ", " conf_rw#get) )))

type t = string

type sandboxer = {
  init: network:bool -> t;
  mount: t -> flag:[`Rw | `Ro] -> string -> t;
  setenv: t -> string -> string -> t;
  unsetenv: t -> string -> t;
  cmd: t -> string -> string;
}

let disabled =
  {
    init= (fun ~network:_ -> "");
    mount= (fun t ~flag:_ _ -> t);
    setenv= (fun t _ _ -> t);
    unsetenv= (fun t _ -> t);
    cmd= (fun _ cmd -> cmd);
  }

let bwrap =
  {
    init=
      (fun ~network ->
        Printf.sprintf "--new-session %s"
          (if network then "" else "--unshare-net"));
    mount=
      (fun t ~flag path ->
        match flag with
          | `Ro ->
              Printf.sprintf "%s --ro-bind %S %S" t path path
          | `Rw ->
              Printf.sprintf "%s --bind %S %S" t path path);
    setenv= Printf.sprintf "%s --setenv %S %S";
    unsetenv= Printf.sprintf "%s --unsetenv %S";
    cmd=
      (fun opts cmd ->
        let binary = Utils.which ~path:Configure.path conf_binary#get in
        let cmd =
          if conf_shell#get then
            Printf.sprintf "%s -c %S" conf_shell_path#get cmd
          else cmd
        in
        Printf.sprintf "%s %s --proc /proc --dev /dev %s" binary opts cmd);
  }

let cmd ?rw ?ro ?network cmd =
  let sandboxer =
    (* This is intended to be extendable with more tools in the
       future.. *)
    match conf_sandbox#get with
      | "disabled" ->
          disabled
      | _ when Lazy.force has_binary ->
          bwrap
      | _ ->
          disabled
  in
  let f d v = match v with None -> d | Some v -> v in
  let rw = f conf_rw#get rw in
  let ro = f conf_ro#get ro in
  let network = f conf_network#get network in
  let t = sandboxer.init ~network in
  let t =
    List.fold_left
      (fun t (lbl, v) -> sandboxer.setenv t lbl v)
      t (get_setenv ())
  in
  let t = List.fold_left sandboxer.unsetenv t conf_unsetenv#get in
  let t =
    List.fold_left (fun t path -> sandboxer.mount t ~flag:`Ro path) t ro
  in
  let t =
    List.fold_left (fun t path -> sandboxer.mount t ~flag:`Rw path) t rw
  in
  let cmd = sandboxer.cmd t cmd in
  log#debug "Command: %s" cmd ;
  cmd
