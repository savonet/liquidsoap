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

(* Inspired of https://github.com/johnelse/ocaml-irc-client/blob/main/examples/example2_unix.ml *)

module C = Irc_client_unix
module M = Irc_message

let log = Log.make ["irc"]

module List = struct
  include List

  (** Take the n first elements. *)
  let rec heads n = function
    | _ when n <= 0 -> []
    | [] -> []
    | x :: l -> x :: heads (n - 1) l
end

let irc = Lang.add_module "irc"

let _ =
  Lang.add_builtin ~base:irc "channel" ~category:`String
    ~descr:"Contents of an IRC channel."
    ~examples:
      [
        {|
# Display messages in the #liquidsoap-test room over a video
s = single("test.mp4")
s = video.add_text.native(irc.channel(channel="#liquidsoap-test"), s)
|};
      ]
    [
      ( "server",
        Lang.string_t,
        Some (Lang.string "irc.libera.chat"),
        Some "IRC server." );
      ("port", Lang.int_t, Some (Lang.int 6667), Some "Port for IRC server.");
      ( "channel",
        Lang.string_t,
        Some (Lang.string "#liquidsoap"),
        Some "IRC chan to join." );
      ("nick", Lang.string_t, Some (Lang.string "liquidbot"), Some "Nickname.");
      ("limit", Lang.int_t, Some (Lang.int 10), Some "Limit to n last messages");
    ]
    (Lang.fun_t [] Lang.string_t)
    (fun p ->
      let server = List.assoc "server" p |> Lang.to_string in
      let port = List.assoc "port" p |> Lang.to_int in
      let nick = List.assoc "nick" p |> Lang.to_string in
      let channel = List.assoc "channel" p |> Lang.to_string in
      let limit = List.assoc "limit" p |> Lang.to_int in
      let s = ref [] in
      let connect () =
        log#info "Connecting to %s:%d..." server port;
        C.connect_by_name ~server ~port ~nick ()
      in
      let connected connection =
        log#info "Connected";
        C.send_join ~connection ~channel;
        C.send_privmsg ~connection ~target:channel ~message:"Hi everybody!"
      in
      let callback _ result =
        match result with
          | Result.Ok { M.command = M.PRIVMSG (target, data); prefix }
            when target = channel ->
              let from =
                match prefix with
                  | None -> "?"
                  | Some prefix -> (
                      match String.index_opt prefix '!' with
                        | Some n -> String.sub prefix 0 n
                        | None -> "?")
              in
              let msg = Printf.sprintf "<%s> %s" from data in
              log#debug "Message from %s: %s"
                (Option.value ~default:"?" prefix)
                msg;
              s := List.heads limit (msg :: !s)
          | _ -> ()
      in
      let _ =
        (* TODO: I feel that this is not entirely clean: we should wait for
           startup and clean the thread in the end... *)
        Tutils.create
          (fun () ->
            C.reconnect_loop ~reconnect:true ~after:30 ~connect ~f:connected
              ~callback ())
          () "irc.channel"
      in
      let s () = String.concat "\n" (List.rev !s) in
      Lang.val_fun [] (fun _ -> Lang.string (s ())))
