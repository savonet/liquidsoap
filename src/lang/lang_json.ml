(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

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

open Lang_builtins

let rec to_json v =
  match v.Lang.value with
    | Lang.Unit -> Json_type.Build.null
    | Lang.Bool b -> Json_type.Build.bool b
    | Lang.Int  i -> Json_type.Build.int i
    | Lang.String s -> Json_type.Build.string s
    | Lang.Float  f -> Json_type.Build.float f
    | Lang.List   l ->
        (* Convert (string*'a) list to object *)
        begin 
         try
          match l with
            | x :: _ ->
               begin
                match x.Lang.value with
                  | Lang.Product (x,_) ->
                     begin
                      match x.Lang.value with
                        | Lang.String _ ->
                           let l = 
                            List.map (fun x ->
                                         let (x,y) = Lang.to_product x in
                                         Lang.to_string x,to_json y)
                            l
                           in
                           Json_type.Build.objekt l
                        | _ -> raise Not_found
                     end
                  | _ -> raise Not_found
               end
            | _ -> raise Not_found
         with Not_found ->
               Json_type.Build.list to_json l
        end
    | Lang.Product (p,q) -> 
       Json_type.Build.array [(to_json p);(to_json q)]
    | Lang.Source _ -> Json_type.Build.string "<source>"
    | Lang.Ref v -> Json_type.Build.objekt ["reference",to_json !v]
    | Lang.Encoder e -> Json_type.Build.string
                              (Encoder.string_of_format e)
    | Lang.Request _ -> Json_type.Build.string "<request>"
    | Lang.FFI _
    | Lang.Fun _ -> Json_type.Build.string "<fun>"

let () =
  add_builtin "json_of" ~cat:Liq
    ~descr:"Convert a value to a json string." 
     ["",Lang.univ_t 1,None,None] Lang.string_t
    (fun p ->
      let v = to_json (List.assoc "" p) in
      Lang.string (Json_io.string_of_json ~recursive:true v))
