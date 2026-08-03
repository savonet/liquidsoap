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

(** Marshalling of an ffmpeg option set (a filter's or a bitstream filter's)
    into liquidsoap operator arguments, and back into `Avutil.Options` args. *)

exception No_value_for_option

(* GADT to encode the relationship between ground types and their converters *)
type 'a ground_opt_utils = {
  lang_type : Lang.t;
  to_string : 'a -> string;
  from_value : Lang.value -> 'a;
}

type ground_opt_descr =
  | Ground_opt :
      'a ground_opt_utils * 'a Avutil.Options.entry
      -> ground_opt_descr

let get_ground_converter : Avutil.Options.ground -> ground_opt_descr = function
  | `Int s ->
      Ground_opt
        ( {
            lang_type = Lang.int_t;
            to_string = string_of_int;
            from_value = Lang.to_int;
          },
          s )
  | `Flags s | `Int64 s | `UInt64 s | `Duration s ->
      Ground_opt
        ( {
            lang_type = Lang.int_t;
            to_string = Int64.to_string;
            from_value = (fun v -> Int64.of_int (Lang.to_int v));
          },
          s )
  | `Float s | `Double s ->
      Ground_opt
        ( {
            lang_type = Lang.float_t;
            to_string = string_of_float;
            from_value = Lang.to_float;
          },
          s )
  | `Rational s ->
      Ground_opt
        ( {
            lang_type = Lang.string_t;
            to_string =
              (fun { Avutil.num; den } -> Printf.sprintf "%i/%i" num den);
            from_value =
              (fun v ->
                let x = Lang.to_string v in
                match String.split_on_char '/' x with
                  | [num; den] ->
                      {
                        Avutil.num = int_of_string num;
                        den = int_of_string den;
                      }
                  | _ -> assert false);
          },
          s )
  | `Bool s ->
      Ground_opt
        ( {
            lang_type = Lang.bool_t;
            to_string = string_of_bool;
            from_value = Lang.to_bool;
          },
          s )
  | `String s | `Binary s | `Dict s | `Image_size s | `Video_rate s | `Color s
    ->
      Ground_opt
        ( {
            lang_type = Lang.string_t;
            to_string = (fun x -> x);
            from_value = Lang.to_string;
          },
          s )
  | `Pixel_fmt s ->
      Ground_opt
        ( {
            lang_type = Lang.string_t;
            to_string =
              (fun p ->
                match Avutil.Pixel_format.to_string p with
                  | None -> "none"
                  | Some p -> p);
            from_value =
              (fun v -> Avutil.Pixel_format.of_string (Lang.to_string v));
          },
          s )
  | `Sample_fmt s ->
      Ground_opt
        ( {
            lang_type = Lang.string_t;
            to_string =
              (fun p ->
                match Avutil.Sample_format.get_name p with
                  | None -> "none"
                  | Some p -> p);
            from_value = (fun v -> Avutil.Sample_format.find (Lang.to_string v));
          },
          s )
  | `Channel_layout s ->
      Ground_opt
        ( {
            lang_type = Lang.string_t;
            to_string = Avutil.Channel_layout.get_description;
            from_value =
              (fun v -> Avutil.Channel_layout.find (Lang.to_string v));
          },
          s )

let mk_options options =
  Avutil.Options.(
    let mk_opt ~t ~to_string ~from_value name help { default; min; max; values }
        =
      let desc =
        let string_of_value (name, value) =
          Printf.sprintf "%s (%s)" (to_string value) name
        in
        let string_of_values values =
          String.concat ", " (List.map string_of_value values)
        in
        match (help, default, values) with
          | Some help, None, [] -> Some help
          | Some help, _, _ ->
              let values =
                if values = [] then None
                else
                  Some
                    (Printf.sprintf "possible values: %s"
                       (string_of_values values))
              in
              let default =
                Option.map
                  (fun default ->
                    Printf.sprintf "default: %s" (to_string default))
                  default
              in
              let l =
                List.fold_left
                  (fun l -> function Some v -> v :: l | None -> l)
                  [] [values; default]
              in
              Some (Printf.sprintf "%s. (%s)" help (String.concat ", " l))
          | None, None, _ :: _ ->
              Some
                (Printf.sprintf "Possible values: %s" (string_of_values values))
          | None, Some v, [] ->
              Some (Printf.sprintf "Default: %s" (to_string v))
          | None, Some v, _ :: _ ->
              Some
                (Printf.sprintf "Default: %s, possible values: %s" (to_string v)
                   (string_of_values values))
          | None, None, [] -> None
      in
      let opt = (name, Lang.nullable_t t, Some Lang.null, desc) in
      let getter p l =
        try
          let v = List.assoc name p in
          let v =
            match Lang.to_option v with
              | None -> raise No_value_for_option
              | Some v -> v
          in
          let x =
            try from_value v
            with _ -> raise (Error.Invalid_value (v, "Invalid value", []))
          in
          (match min with
            | Some m when x < m ->
                raise
                  (Error.Invalid_value
                     ( v,
                       Printf.sprintf "%s must be more than %s" name
                         (to_string m),
                       [] ))
            | _ -> ());
          (match max with
            | Some m when m < x ->
                raise
                  (Error.Invalid_value
                     ( v,
                       Printf.sprintf "%s must be less than %s" name
                         (to_string m),
                       [] ))
            | _ -> ());
          (match values with
            | _ :: _ when List.find_opt (fun (_, v) -> v = x) values = None ->
                raise
                  (Error.Invalid_value
                     ( v,
                       Printf.sprintf "%s should be one of: %s" name
                         (String.concat ", "
                            (List.map (fun (_, v) -> to_string v) values)),
                       [] ))
            | _ -> ());
          let x =
            match default with
              | Some v
                when to_string v = Int64.to_string Int64.max_int
                     && to_string x = string_of_int max_int ->
                  `Int64 Int64.max_int
              | Some v
                when to_string v = Int64.to_string Int64.min_int
                     && to_string x = string_of_int min_int ->
                  `Int64 Int64.min_int
              | _ -> `String (to_string x)
          in
          `Pair (name, x) :: l
        with No_value_for_option -> l
      in
      (opt, getter)
    in
    let mk_opt (p, getter) { name; help; spec } =
      let mk_opt_outer = mk_opt in
      let mk_opt ~t ~to_string ~from_value spec =
        let opt, get = mk_opt_outer ~t ~to_string ~from_value name help spec in
        let getter p l = get p (getter p l) in
        (opt :: p, getter)
      in
      match spec with
        | #Avutil.Options.ground as g ->
            let (Ground_opt (conv, s)) = get_ground_converter g in
            mk_opt ~t:conv.lang_type ~to_string:conv.to_string
              ~from_value:conv.from_value s
        | `Array g ->
            let (Ground_opt (conv, _)) = get_ground_converter g in
            let ground_t = conv.lang_type in
            let ground_to_string = conv.to_string in
            let ground_from_value = conv.from_value in
            let t = Lang.list_t ground_t in
            let array_to_string values =
              String.concat ", " (List.map ground_to_string values)
            in
            let array_from_value v =
              List.map ground_from_value (Lang.to_list v)
            in
            let dummy_spec =
              {
                Avutil.Options.default = None;
                min = None;
                max = None;
                values = [];
              }
            in
            let opt, _base_getter =
              mk_opt_outer ~t ~to_string:array_to_string
                ~from_value:array_from_value name help dummy_spec
            in
            let new_getter p l =
              try
                let v = List.assoc name p in
                let v =
                  match Lang.to_option v with
                    | None -> raise No_value_for_option
                    | Some v -> v
                in
                let values =
                  try array_from_value v
                  with _ ->
                    raise (Error.Invalid_value (v, "Invalid value", []))
                in
                let array_args =
                  List.map
                    (fun value -> `String (ground_to_string value))
                    values
                in
                `Pair (name, `Array array_args) :: getter p l
              with No_value_for_option -> getter p l
            in
            (opt :: p, new_getter)
    in
    List.fold_left mk_opt ([], fun _ x -> x) (Avutil.Options.opts options))
