open Js_of_ocaml

type t = Js.regExp Js.t
type sub = Js.match_result Js.t
type flag = [ `i | `g | `s | `m ]

let string_of_flag = function `i -> "i" | `g -> "g" | `s -> "s" | `m -> "m"

let flags_of_flags flags =
  Js.string (String.concat "" (List.map string_of_flag flags))

let regexp ?(flags = []) s =
  new%js Js.regExp_withFlags (Js.string s) (flags_of_flags flags)

let regexp_or ?flags l = regexp ?flags (String.concat "\\|" l)

let pat_of_rex rex pat =
  match (pat, rex) with
    | _, Some r -> r
    | Some p, None -> regexp p
    | None, None -> failwith "At least one of pat or rex must be provided!"

let split ?pat ?rex s =
  let rex = pat_of_rex rex pat in
  let split = (Js.string s)##split_regExp rex in
  let split = Js.str_array split in
  Array.to_list (Array.map Js.to_string (Js.to_array split))

let exec ?pat ?rex s =
  let rex = pat_of_rex rex pat in
  let s = Js.string s in
  let ret =
    Js.Opt.case (rex##exec s) (fun () -> raise Not_found) (fun x -> x)
  in
  Js.match_result ret

let test ?pat ?rex s =
  let rex = pat_of_rex rex pat in
  Js.to_bool (rex##test (Js.string s))

let num_of_subs sub = sub##.length

let get_substring sub pos =
  Js.to_string (Option.get (Js.Optdef.to_option (Js.array_get sub pos)))

let substitute ?pat ?rex ~subst s =
  let rex = pat_of_rex rex pat in
  let subst a = Js.string (subst (Js.to_string a)) in
  let subst = Js.wrap_callback subst in
  Js.to_string ((Js.Unsafe.coerce (Js.string s))##replace rex subst)

let substitute_first ?pat ?rex ~subst s =
  let rex = pat_of_rex rex pat in
  let flags : Js.js_string Js.t = (Js.Unsafe.coerce (Js.string s))##.flags in
  let flags = flags##replace_string (Js.string "g") (Js.string "") in
  let rex = new%js Js.regExp_withFlags rex##.source flags in
  substitute ~rex ~subst s
