type entry = {
  uri : string;
  duration : float;
  discontinuous : bool;
  init_uri : string option;
  tags : string list;
}

type media = {
  target : int;
  independent : bool;
  media_sequence : int;
  discontinuity_sequence : int;
  extra_tags : string list;
  entries : entry list;
}

let version = 6

(* RFC 3986 pchar: unreserved, sub-delims, ':' and '@'. *)
let is_pchar = function
  | 'A' .. 'Z'
  | 'a' .. 'z'
  | '0' .. '9'
  | '-' | '.' | '_' | '~' | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ','
  | ';' | '=' | ':' | '@' ->
      true
  | _ -> false

let encode_segment s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if is_pchar c then Buffer.add_char b c
      else Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents b

let uri ~prefix name =
  let path =
    String.concat "/" (List.map encode_segment (String.split_on_char '/' name))
  in
  match prefix with
    | "" -> path
    | p when p.[String.length p - 1] = '/' -> p ^ path
    | p -> p ^ "/" ^ path

let lines l = String.concat "" (List.map (fun line -> line ^ "\r\n") l)

let media m =
  let header =
    [
      "#EXTM3U";
      Printf.sprintf "#EXT-X-TARGETDURATION:%d" m.target;
      Printf.sprintf "#EXT-X-VERSION:%d" version;
    ]
    @ (if m.independent then ["#EXT-X-INDEPENDENT-SEGMENTS"] else [])
    @ [
        Printf.sprintf "#EXT-X-MEDIA-SEQUENCE:%d" m.media_sequence;
        Printf.sprintf "#EXT-X-DISCONTINUITY-SEQUENCE:%d"
          m.discontinuity_sequence;
      ]
    @ m.extra_tags
  in
  let _, segments =
    List.fold_left
      (fun (previous_init, acc) (i, e) ->
        let discontinuity =
          if i > 0 && e.discontinuous then ["#EXT-X-DISCONTINUITY"] else []
        in
        let map =
          match e.init_uri with
            | Some init when i = 0 || previous_init <> e.init_uri ->
                [Printf.sprintf "#EXT-X-MAP:URI=\"%s\"" init]
            | _ -> []
        in
        ( e.init_uri,
          acc @ discontinuity @ map @ e.tags
          @ [Printf.sprintf "#EXTINF:%.3f," e.duration; e.uri] ))
      (None, [])
      (List.mapi (fun i e -> (i, e)) m.entries)
  in
  lines (header @ segments)
