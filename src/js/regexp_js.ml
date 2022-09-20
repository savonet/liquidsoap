open Js_of_ocaml

type sub = Liquidsoap_lang.Regexp.sub = {
  matches : string option list;
  groups : (string * string) list;
}

class type match_result =
  object
    inherit Js.match_result
    method groups : Js.Unsafe.any Js.optdef Js.readonly_prop
  end

let string_of_flag = function `i -> "i" | `g -> "g" | `s -> "s" | `m -> "m"

let flags_of_flags flags =
  Js.string (String.concat "" (List.map string_of_flag flags))

let make ?(flags = []) s =
  let rex = new%js Js.regExp_withFlags (Js.string s) (flags_of_flags flags) in
  object
    method split s =
      let split = (Js.string s)##split_regExp rex in
      let split = Js.str_array split in
      Array.to_list (Array.map Js.to_string (Js.to_array split))

    method exec s =
      let s = Js.string s in
      let ret =
        Js.Opt.case (rex##exec s) (fun () -> raise Not_found) (fun x -> x)
      in
      let sub : match_result Js.t = Js.Unsafe.coerce (Js.match_result ret) in
      let matches =
        List.init sub##.length (fun pos ->
            Option.map Js.to_string (Js.Optdef.to_option (Js.array_get sub pos)))
      in
      let groups =
        Js.Optdef.case sub##.groups
          (fun () -> [])
          (fun groups ->
            let names = Js.to_array (Js.object_keys groups) in
            Array.fold_left
              (fun cur key ->
                Js.Optdef.case (Js.Unsafe.get groups key)
                  (fun () -> cur)
                  (fun value -> (Js.to_string key, Js.to_string value) :: cur))
              [] names)
      in
      { matches; groups }

    method test s = Js.to_bool (rex##test (Js.string s))

    method substitute ~subst s =
      let subst a = Js.string (subst (Js.to_string a)) in
      let subst = Js.wrap_callback subst in
      Js.to_string ((Js.Unsafe.coerce (Js.string s))##replace rex subst)
  end
