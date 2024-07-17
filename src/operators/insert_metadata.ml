(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

open Source
open Genlex
exception Error

class insert_metadata source =
object (self)
  inherit operator [source] as super

  method stype = source#stype
  method is_ready = source#is_ready
  method remaining = source#remaining
  method abort_track = source#abort_track

  val mutable metadata = None
  val mutable ns = []

  method private wake_up l =
    super#wake_up l ;
    if ns = [] then
      ns <- Server.register [self#id] "insert_metadata" ;
    self#set_id (Server.to_string ns) ;
    Server.add ~ns "insert" ~usage:"insert key1=\"val1\",key2=\"val2\",.."
      (fun s ->
         let l = String.length s in
         let pos = ref 0 in
         let str =
           Stream.from (fun i ->
                          pos := i ;
                          if i<l then Some s.[i] else None)
         in
         let lexer = make_lexer [",";"="] str in
         let m = Hashtbl.create 10 in
         let state = ref `Ident in
           try
             while true do
               match Stream.next lexer with
                 | Kwd ","   when `Comma = !state ->
                     state := `Ident
                 | Ident key when `Ident = !state ->
                     state := `Internal ;
                     begin match Stream.next lexer with
                       | Kwd "=" -> begin match Stream.next lexer with
                           | String s ->
                               Hashtbl.add m key s ;
                               state := `Comma
                           | _ -> raise Error
                         end
                       | _ -> raise Error
                     end
                 | _ -> raise Error
             done ;
             assert false
           with
             | Stream.Failure when `Indent = !state || `Comma = !state ->
                 metadata <- Some m ;
                 "Done"
             | Error | Stream.Failure ->
                 "Syntax error: use key1=\"val1\",key2=\"val2\",..")

  method get_frame buf =
    let p = Frame.position buf in
      source#get buf ;
      match metadata with
        | Some m ->
            Frame.set_metadata buf p m ;
            metadata <- None
        | None -> ()

end

let register =
  Lang.add_operator "insert_metadata" [ "", Lang.source_t, None, None ]
    ~category:Lang.SoundProcessing
    ~descr:("Interactively insert metadata using the command "^
            "<id>.insert key1=\"val1\",key2=\"val2\",...")
    (fun p ->
       let source = Lang.to_source (Lang.assoc "" 1 p) in
         ((new insert_metadata source):>source))
