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

(** User-friendly representation of types. *)

(** Show generalized variables in records. *)
let show_record_schemes = ref true

(** Use globally unique names for existential variables. *)
let global_evar_names = ref false

open Type_base
include R

type t = Type_base.constr R.t

(** Given a position, find the relevant excerpt. *)
let excerpt pos =
  let { Pos.fname; lstart; lstop; cstart; cstop } = Pos.unpack pos in
  try
    let lines =
      let ic = open_in fname in
      Fun.protect
        ~finally:(fun () -> close_in ic)
        (fun () ->
          let n = ref 1 in
          while !n < lstart do
            ignore (input_line ic);
            incr n
          done;
          let lines = ref [] in
          while !n <= lstop do
            lines := input_line ic :: !lines;
            incr n
          done;
          lines)
    in
    let lines = Array.of_list (List.rev !lines) in
    let lines =
      let n = Array.length lines in
      if Array.length lines > 5 then
        [| lines.(0); lines.(1); "..."; lines.(n - 2); lines.(n - 1) |]
      else lines
    in
    let insert_at x n s =
      let s1 = String.sub s 0 n in
      let s2 = String.sub s n (String.length s - n) in
      s1 ^ x ^ s2
    in
    (* The order is important here because both lines might be the same. *)
    lines.(Array.length lines - 1) <-
      insert_at (Console.stop_color ()) cstop lines.(Array.length lines - 1);
    lines.(0) <- insert_at (Console.start_color [`red]) cstart lines.(0);
    let lines = Array.to_list lines in
    let s = String.concat "\n" lines ^ "\n" in
    Some s
  with _ -> None

let excerpt_opt = function Some pos -> excerpt pos | None -> None

(** Given a strictly positive integer, generate a name in [a-z]+: a, b, ... z,
    aa, ab, ... az, ba, ... *)
let name =
  let base = 26 in
  let c i = char_of_int (int_of_char 'a' + i - 1) in
  let add i suffix = Printf.sprintf "%c%s" (c i) suffix in
  let rec n suffix i =
    if i <= base then add i suffix
    else (
      let head = i mod base in
      let head = if head = 0 then base else head in
      n (add head suffix) ((i - head) / base))
  in
  n ""

(** Generate a globally unique name for evars (used for debugging only). *)
let evar_global_name =
  let evars = Hashtbl.create 10 in
  let n = ref 0 in
  fun i ->
    try Hashtbl.find evars i
    with Not_found ->
      incr n;
      let name = String.uppercase_ascii (name !n) in
      Hashtbl.replace evars i name;
      name

(** Compute the structure that a term represents, given the list of universally
    quantified variables. Also takes care of computing the printing name of
    variables, including constraint symbols, which are removed from constraint
    lists. It supports a mechanism for filtering out parts of the type, which
    are then translated as `Ellipsis. *)
let make ?(filter_out = fun _ -> false) ?(generalized = []) t : t =
  let split_constr c =
    List.fold_left (fun (s, constraints) c -> (s, c :: constraints)) ("", []) c
  in
  let uvar g var =
    let constr_symbols, c =
      split_constr (Constraints.elements var.constraints)
    in
    let rec index n = function
      | v :: tl ->
          if Var.eq v var then Printf.sprintf "'%s%s" constr_symbols (name n)
          else index (n + 1) tl
      | [] -> assert false
    in
    let v = index 1 (List.rev g) in
    (* let v = Printf.sprintf "'%d" i in *)
    `UVar (v, Constraints.of_list c)
  in
  let counter =
    let c = ref 0 in
    fun () ->
      incr c;
      !c
  in
  let evars = Hashtbl.create 10 in
  let evar var =
    let constr_symbols, c =
      split_constr (Constraints.elements var.constraints)
    in
    if !global_evar_names || !debug || !debug_levels then (
      let v =
        Printf.sprintf "'%s%s" constr_symbols (evar_global_name var.name)
      in
      let v =
        if !debug_levels then (
          let level = var.level in
          let level = if level = max_int then "∞" else string_of_int level in
          Printf.sprintf "%s[%s]" v level)
        else v
      in
      `EVar (v, Constraints.of_list c))
    else (
      let s =
        try Hashtbl.find evars var.name
        with Not_found ->
          let name = String.uppercase_ascii (name (counter ())) in
          Hashtbl.replace evars var.name name;
          name
      in
      `EVar (Printf.sprintf "'%s%s" constr_symbols s, Constraints.of_list c))
  in
  let rec repr g t =
    if filter_out t then `Ellipsis
    else (
      match t.descr with
        | Int -> `Constr ("int", [])
        | Float -> `Constr ("float", [])
        | String -> `Constr ("string", [])
        | Bool -> `Constr ("bool", [])
        | Never -> `Constr ("never", [])
        | Custom c -> c.repr repr g c.typ
        | Getter t -> `Getter (repr g t)
        | List { t; json_repr } -> `List (repr g t, json_repr)
        | Tuple l -> `Tuple (List.map (repr g) l)
        | Nullable t -> `Nullable (repr g t)
        | Meth
            { meth = { name = l; optional; scheme = g', u; json_name }; t = v }
          ->
            let gen =
              List.map
                (fun v -> match uvar (g' @ g) v with `UVar v -> v)
                (List.sort_uniq compare g')
            in
            `Meth
              ( R.
                  {
                    name = l;
                    optional;
                    scheme = (gen, repr (g' @ g) u);
                    json_name;
                  },
                repr g v )
        | Constr { constructor; params } ->
            `Constr (constructor, List.map (fun (l, t) -> (l, repr g t)) params)
        | Arrow { args; t } ->
            `Arrow
              ( List.map (fun (opt, lbl, t) -> (opt, lbl, repr g t)) args,
                repr g t )
        | Var { contents = Free var } ->
            if List.exists (Var.eq var) g then uvar g var else evar var
        | Var { contents = Link (`Covariant, t) } when !debug || !debug_variance
          ->
            `Debug ("[>", repr g t, "]")
        | Var { contents = Link (_, t) } -> repr g t)
  in
  repr generalized t

(** Print a type representation. Unless in debug mode, variable identifiers are
    not shown, and variable names are generated. Names are only meaningful over
    one printing, as they are re-used. *)
let print f t =
  (* Display the type and return the list of variables that occur in it.
     The [par] params tells whether (..)->.. should be surrounded by
     parenthesis or not. *)
  let rec print ~par vars : t -> DS.t = function
    | `Constr (name, [(_, (`Meth _ as record_type))])
      when name = "source" || name = "format" ->
        Format.open_box (1 + String.length name);
        Format.fprintf f "%s(" name;
        let rec extract fields = function
          | `Meth ({ R.name = field }, base_type)
            when List.mem_assoc (Some field) fields ->
              extract fields base_type
          | `Meth ({ R.scheme = _, `Constr ("never", _) }, base_type) ->
              extract fields base_type
          | `Meth (R.{ name = field; optional; scheme = _, ty }, base_type) ->
              extract ((Some field, (optional, ty)) :: fields) base_type
          | base_type -> (fields, base_type)
        in
        let fields, base_type = extract [] record_type in
        let fields =
          List.sort (fun (l, _) (l', _) -> Stdlib.compare l l') fields
        in
        let fields =
          match (base_type, fields) with
            | `Tuple [], _ -> fields
            | v, _ -> fields @ [(None, (false, v))]
        in
        let _, vars =
          List.fold_left
            (fun (first, vars) (lbl, (optional, t)) ->
              if not first then Format.fprintf f ",@ ";
              ignore
                (Option.map
                   (Format.fprintf f "%s%s=" (if optional then "?" else ""))
                   lbl);
              let vars = print ~par:false vars t in
              (false, vars))
            (true, vars) fields
        in
        Format.fprintf f ")";
        Format.close_box ();
        vars
    | `Constr (name, []) ->
        Format.fprintf f "%s" name;
        vars
    | `Constr ("none", _) ->
        Format.fprintf f "none";
        vars
    | `Constr (name, params) ->
        Format.open_box (1 + String.length name);
        Format.fprintf f "%s(" name;
        let vars = print_list vars params in
        Format.fprintf f ")";
        Format.close_box ();
        vars
    | `Tuple [] ->
        Format.fprintf f "unit";
        vars
    | `Tuple l ->
        if par then Format.fprintf f "@[<1>(" else Format.fprintf f "@[<0>";
        let rec aux vars = function
          | [a] -> print ~par:true vars a
          | a :: l ->
              let vars = print ~par:true vars a in
              Format.fprintf f " *@ ";
              aux vars l
          | [] -> assert false
        in
        let vars = aux vars l in
        if par then Format.fprintf f ")@]" else Format.fprintf f "@]";
        vars
    | `Nullable t ->
        let vars = print ~par:true vars t in
        Format.fprintf f "?";
        vars
    | `Meth (R.{ name = l; scheme = _, a }, b) as t ->
        if not !debug then (
          (* Find all methods. *)
          let rec aux = function
            | `Meth (R.{ name = l; optional; scheme = t; json_name }, u) ->
                let m, u = aux u in
                ((l, optional, t, json_name) :: m, u)
            | u -> ([], u)
          in
          let m, t = aux t in
          (* Filter out duplicates. *)
          let rec aux = function
            | (l, o, t, json_name) :: m ->
                (l, o, t, json_name)
                :: aux (List.filter (fun (l', _, _, _) -> l <> l') m)
            | [] -> []
          in
          let m = aux m in
          (* Put latest addition last. *)
          (* let m = List.rev m in *)
          (* Sort methods according to label. *)
          let m =
            List.sort (fun (l, _, _, _) (l', _, _, _) -> compare l l') m
          in
          (* First print the main value. *)
          let vars =
            if t = `Tuple [] then (
              Format.fprintf f "@,@[<hv 2>{@,";
              vars)
            else (
              let vars = print ~par:true vars t in
              Format.fprintf f "@,@[<hv 2>.{@,";
              vars)
          in
          let vars =
            if m = [] then vars
            else (
              let rec gen = function
                | (x, _) :: g -> x ^ "." ^ gen g
                | [] -> ""
              in
              let gen g =
                if !show_record_schemes then gen (List.sort compare g) else ""
              in
              let rec aux vars = function
                | [(l, optional, (g, t), Some json_name)] ->
                    let optional = if optional then "?" else "" in
                    Format.fprintf f "%s%s as %s%s : %s"
                      (Lang_string.quote_utf8_string json_name)
                      optional l optional (gen g);
                    print ~par:true vars t
                | [(l, optional, (g, t), None)] ->
                    let optional = if optional then "?" else "" in
                    Format.fprintf f "%s%s : %s" l optional (gen g);
                    print ~par:false vars t
                | (l, optional, (g, t), Some json_name) :: m ->
                    let optional = if optional then "?" else "" in
                    Format.fprintf f "%s%s as %s%s : %s"
                      (Lang_string.quote_utf8_string json_name)
                      optional l optional (gen g);
                    let vars = print ~par:false vars t in
                    Format.fprintf f ",@ ";
                    aux vars m
                | (l, optional, (g, t), None) :: m ->
                    let optional = if optional then "?" else "" in
                    Format.fprintf f "%s%s : %s" l optional (gen g);
                    let vars = print ~par:false vars t in
                    Format.fprintf f ",@ ";
                    aux vars m
                | [] -> assert false
              in
              aux vars m)
          in
          Format.fprintf f "@]@,}";
          vars)
        else (
          let vars = print ~par:true vars b in
          Format.fprintf f ".{%s = " l;
          let vars = print ~par:false vars a in
          Format.fprintf f "}";
          vars)
    | `List (t, `Tuple) ->
        Format.fprintf f "@[<1>[";
        let vars = print ~par:false vars t in
        Format.fprintf f "]@]";
        vars
    | `List (t, `Object) ->
        Format.fprintf f "@[<1>[";
        let vars = print ~par:false vars t in
        Format.fprintf f "] as json.object@]";
        vars
    | `Getter t ->
        Format.fprintf f "{";
        let vars = print ~par:false vars t in
        Format.fprintf f "}";
        vars
    | (`EVar (_, c) | `UVar (_, c))
      when Constraints.cardinal c = 1
           && (Constraints.choose c).univ_descr <> None ->
        let constr = Constraints.choose c in
        Format.fprintf f "%s" (Option.get constr.univ_descr);
        vars
    | `EVar (name, c) | `UVar (name, c) ->
        Format.fprintf f "%s" name;
        if not (Constraints.is_empty c) then DS.add (name, c) vars else vars
    | `Arrow (p, t) ->
        if par then Format.fprintf f "@[<hov 1>("
        else Format.fprintf f "@[<hov 0>";
        Format.fprintf f "@[<1>(";
        let _, vars =
          List.fold_left
            (fun (first, vars) (opt, lbl, kind) ->
              if not first then Format.fprintf f ",@ ";
              if opt then Format.fprintf f "?";
              if lbl <> "" then Format.fprintf f "%s : " lbl;
              let vars = print ~par:true vars kind in
              (false, vars))
            (true, vars) p
        in
        Format.fprintf f ")@] ->@ ";
        let vars = print ~par:false vars t in
        if par then Format.fprintf f ")@]" else Format.fprintf f "@]";
        vars
    | `Ellipsis ->
        Format.fprintf f "_";
        vars
    | `Range_Ellipsis ->
        Format.fprintf f "...";
        vars
    | `Debug (a, b, c) ->
        Format.fprintf f "%s" a;
        let vars = print ~par:false vars b in
        Format.fprintf f "%s" c;
        vars
  and print_list ?(first = true) ?(acc = []) vars = function
    | [] -> vars
    | (_, x) :: l ->
        if not first then Format.fprintf f ",";
        let vars = print ~par:false vars x in
        print_list ~first:false ~acc:(x :: acc) vars l
  in
  Format.fprintf f "@[";
  begin match t with
    (* We're only printing a variable: ignore its [repr]esentation. *)
    | `EVar (_, c) when not (Constraints.is_empty c) ->
        Format.fprintf f "something that is %s"
          (String.concat " and "
             (List.map string_of_constr (Constraints.elements c)))
    | `UVar (_, c) when not (Constraints.is_empty c) ->
        Format.fprintf f "anything that is %s"
          (String.concat " and "
             (List.map string_of_constr (Constraints.elements c)))
    (* Print the full thing, then display constraints *)
    | _ ->
        let constraints = print ~par:false DS.empty t in
        let constraints = DS.elements constraints in
        if constraints <> [] then (
          let constraints =
            List.map
              (fun (name, c) ->
                ( name,
                  String.concat " and "
                    (List.map string_of_constr (Constraints.elements c)) ))
              constraints
          in
          let constraints =
            List.stable_sort (fun (_, a) (_, b) -> compare a b) constraints
          in
          let group : ('a * 'b) list -> ('a list * 'b) list = function
            | [] -> []
            | (i, c) :: l ->
                let rec group prev acc = function
                  | [] -> [(List.rev acc, prev)]
                  | (i, c) :: l ->
                      if prev = c then group c (i :: acc) l
                      else (List.rev acc, prev) :: group c [i] l
                in
                group c [i] l
          in
          let constraints = group constraints in
          let constraints =
            List.map
              (fun (ids, c) -> String.concat ", " ids ^ " is " ^ c)
              constraints
          in
          Format.fprintf f "@ @[<2>where@ ";
          Format.fprintf f "%s" (List.hd constraints);
          List.iter (fun s -> Format.fprintf f ",@ %s" s) (List.tl constraints);
          Format.fprintf f "@]")
  end;
  Format.fprintf f "@]"

let to_string t =
  print Format.str_formatter t;
  Format.fprintf Format.str_formatter "@?";
  Format.flush_str_formatter ()

let print_type f t = print f (make t)

let print_scheme f (generalized, t) =
  if !debug then
    List.iter
      (fun v ->
        print f
          (make ~generalized
             (Type_base.make (Var { id = 0; contents = Free v })));
        Format.fprintf f ".")
      generalized;
  print f (make ~generalized t)

(** String representation of a type. *)
let string_of_type ?generalized t = to_string (make ?generalized t)

(* This is filled in there in order to avoid cyclic dependencies. *)
let () = Type_base.to_string_fun := string_of_type

(** String representation of a type scheme. *)
let string_of_scheme (g, t) = string_of_type ~generalized:g t

type explanation = bool * Type_base.t * Type_base.t * t * t

exception Type_error of explanation

let print_type_error ~formatter error_header
    ((flipped, ta, tb, a, b) : explanation) =
  error_header ta.pos;
  match b with
    | `Meth (R.{ name = l; scheme = [], `Ellipsis }, `Ellipsis) when not flipped
      ->
        Format.fprintf formatter
          "this value has no method `%s`@.@[<2>  Its type is %s.@]@." l
          (string_of_type ta)
    | _ ->
        let inferred_pos a =
          let dpos = (deref a).pos in
          if a.pos = dpos then ""
          else (
            match dpos with
              | None -> ""
              | Some p -> " (inferred at " ^ Pos.to_string ~prefix:"" p ^ ")")
        in
        let ta, tb, a, b = if flipped then (tb, ta, b, a) else (ta, tb, a, b) in
        Format.fprintf formatter "this value has type@.@[<2>  %a@]%s@ " print a
          (inferred_pos ta);
        Format.fprintf formatter
          "but it should be a %stype of%s@.@[<2>  %a@]%s@]@."
          (if flipped then "super" else "sub")
          (match tb.pos with
            | None -> ""
            | Some p ->
                Printf.sprintf " the type of the value at %s"
                  (Pos.to_string ~prefix:"" p))
          print b (inferred_pos tb)
