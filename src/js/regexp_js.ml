open Js_of_ocaml

type t = Js.regExp Js.t
type sub = Js.match_result Js.t

let regexp s = new%js Js.regExp_withFlags (Js.string s) (Js.string "g")
let regexp_or l = regexp (String.concat "\\|" l)

let split ~pat s =
  let rex = regexp pat in
  let split = (Js.string s)##split_regExp rex in
  let split = Js.str_array split in
  Array.to_list (Array.map Js.to_string (Js.to_array split))

let exec ?pat ?rex s =
  let rex =
    match (pat, rex) with
      | _, Some r -> r
      | Some p, None -> regexp p
      | None, None -> failwith "At least one of pat or rex must be provided!"
  in
  let s = Js.string s in
  let ret =
    Js.Opt.case (rex##exec s) (fun () -> raise Not_found) (fun x -> x)
  in
  Js.match_result ret

let get_substring sub pos =
  Js.to_string (Option.get (Js.Optdef.to_option (Js.array_get sub pos)))

let substitute ?pat ?rex ~subst s =
  let rex =
    match (pat, rex) with
      | _, Some r -> r
      | Some p, None -> regexp p
      | None, None -> failwith "At least one of pat or rex must be provided!"
  in
  let subst a = Js.string (subst (Js.to_string a)) in
  let subst = Js.wrap_callback subst in
  Js.to_string ((Js.Unsafe.coerce (Js.string s))##replace rex subst)
