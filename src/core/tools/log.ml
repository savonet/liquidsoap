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

(** Logging functions. *)

let conf_recode =
  Dtools.Conf.bool
    ~p:(Dtools.Log.conf#plug "recode")
    ~d:false
    "Recode log entries. Source encoding is set using \
     `settings.charset.encodings`."

let conf_encoding =
  Dtools.Conf.string
    ~p:(conf_recode#plug "encoding")
    ~d:"UTF-8" "Encoding to recode log entries to."

let recode = ref (fun s -> s)

type t =
  < active : int -> bool
  ; f : 'a. int -> ('a, unit, string, unit) format4 -> 'a
  ; critical : 'a. ('a, unit, string, unit) format4 -> 'a
  ; severe : 'a. ('a, unit, string, unit) format4 -> 'a
  ; important : 'a. ('a, unit, string, unit) format4 -> 'a
  ; info : 'a. ('a, unit, string, unit) format4 -> 'a
  ; debug : 'a. ('a, unit, string, unit) format4 -> 'a
  ; level : int
  ; set_level : int -> unit >

let make path : t =
  let colorize colors { Dtools.Log.time; label; level; log } =
    let recode = !recode in
    {
      Dtools.Log.time;
      label = Option.map (fun s -> Console.colorize [`green] (recode s)) label;
      level;
      log = Console.colorize colors (recode log);
    }
  in
  let log = Dtools.Log.make path in
  object (self)
    (** Is that level active (i.e. will it print logs) *)
    method active lvl = log#active lvl

    (** Logging function. *)
    method f : 'a. int -> ('a, unit, string, unit) format4 -> 'a =
      function
      | 1 -> self#critical
      | 2 -> self#severe
      | 3 -> self#important
      | 4 -> self#info
      | 5 -> self#debug
      | v -> log#f v

    (** The program will not function after that. *)
    method critical : 'a. ('a, unit, string, unit) format4 -> 'a =
      log#g ~colorize:(colorize [`red]) 1

    (** The behavior of the program will be strongly affected. *)
    method severe : 'a. ('a, unit, string, unit) format4 -> 'a =
      log#g ~colorize:(colorize [`yellow]) 2

    (** The user should now about this. *)
    method important : 'a. ('a, unit, string, unit) format4 -> 'a =
      log#g ~colorize:(colorize []) 3

    (** The advanced user should be interested in this. *)
    method info : 'a. ('a, unit, string, unit) format4 -> 'a =
      log#g ~colorize:(colorize [`blue]) 4

    (** If you are debugging. *)
    method debug : 'a. ('a, unit, string, unit) format4 -> 'a =
      log#g ~colorize:(colorize [`cyan]) 5

    method level = log#level
    method set_level lvl = log#set_level lvl
  end

let () =
  let log = make ["log"] in
  let set_recode () =
    recode :=
      match Charset_base.of_string conf_encoding#get with
        | out_enc -> (
            fun s ->
              let in_enc = Charset_base.automatic_encoding () in
              try Charset_base.recode_string ~in_enc ~out_enc s
              with exn ->
                log#important "Failed to convert %S: unknown error %s" s
                  (Printexc.to_string exn);
                s)
        | exception _ ->
            log#severe "Invalid target encoding for log conversion: %s"
              conf_encoding#get;
            fun s -> s
  in
  conf_recode#on_change (fun recode -> if recode then set_recode ());
  conf_encoding#on_change (fun _ -> if conf_recode#get then set_recode ())

let conf_console =
  Dtools.Conf.void ~p:(Configure.conf#plug "console") "Console configuration"

let conf_colorize =
  Dtools.Conf.string
    ~p:(conf_console#plug "colorize")
    ~d:
      (match !Console.color_conf with
        | `Auto -> "auto"
        | `Always -> "always"
        | `Never -> "never")
    "Use color in console output when available. One of: \"always\", \"never\" \
     or \"auto\"."

let () =
  let log = make ["console"] in
  conf_colorize#on_change (function
    | "auto" -> Console.color_conf := `Auto
    | "always" -> Console.color_conf := `Always
    | "never" -> Console.color_conf := `Never
    | _ ->
        log#important "Invalid color configuration, using default \"auto\"";
        Console.color_conf := `Auto)
