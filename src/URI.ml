exception Malformed

let allowed_chars = "%a-zA-Z0-9\\._~-"
let allowed_chars_ext = "!\\$&'()\\*\\+,;="^allowed_chars
let allowed_chars_ext_ext = ":@"^allowed_chars_ext
let allowed_chars_ext_ext_ext = "\\?/"^allowed_chars_ext_ext
let r_scheme = "\\([a-zA-Z][a-zA-Z0-9+.-]*\\):"
let r_userinfo =
    "\\(["^allowed_chars_ext^"]+\\(:["^allowed_chars_ext^"]+\\)?\\)@"
let r_hostname = "["^allowed_chars_ext^"]+"
let ip4_num = "[0-2]?[0-9]?[0-9]"
let r_ip4 = ip4_num^"\\."^ip4_num^"\\."^ip4_num^"\\."^ip4_num
let r_ip6 = "\\[[0-9A-Fa-f:]+\\]"
let r_host = r_hostname^"\\|"^r_ip4^"\\|"^r_ip6
let r_port = ":\\([0-9]+\\)"
let r_authority =
    "\\(//\\(\\("^r_userinfo^"\\)?\\("^r_host^"\\)\\("^r_port^"\\)?\\)\\)?"
let r_path = "\\([/"^allowed_chars_ext_ext^"]*\\)"
let r_query = "\\(\\?\\(["^allowed_chars_ext_ext_ext^"]+\\)\\)?"
let r_fragment = "\\(#\\(["^allowed_chars_ext_ext_ext^"]+\\)\\)?"
let scheme_regexp = Str.regexp ("^"^r_scheme)
let uri_regexp =
    Str.regexp ("^"^r_scheme^r_authority^r_path^r_query^r_fragment^"$")

type t = {
  value : string;
  scheme : string;
  authority : string option;
  userinfo : string option;
  host : string option;
  port : int option;
  path : string;
  query : string option;
  fragment : string option;
}

let home_unrelate s = Utils.home_unrelate s

let create uri =
  let uri =
    (* We _must_ have a scheme *)
    if Str.string_match scheme_regexp uri 0 then
      uri
    else
      "file:"^uri
  in
  try
    if Str.string_match uri_regexp uri 0 then
      let scheme = Str.matched_group 1 uri in
      let authority =
        try Some (Str.matched_group 3 uri) with Not_found -> None in
      let userinfo =
        try Some (Str.matched_group 5 uri) with Not_found -> None in
      let host = try Some (Str.matched_group 7 uri) with Not_found -> None in
      let port = try
        Some (int_of_string (Str.matched_group 9 uri))
        with Not_found -> None in
      let path = try Str.matched_group 10 uri with Not_found -> "" in
      let query = try Some (Str.matched_group 12 uri) with Not_found -> None in
      let fragment =
        try Some (Str.matched_group 14 uri) with Not_found -> None in
      let path = if scheme = "file" then home_unrelate path else path in
      {
        value = uri;
        scheme = scheme;
        authority = authority;
        userinfo = userinfo;
        host = host;
        port = port;
        path = path;
        query = query;
        fragment = fragment;
      }
    else
      raise Malformed
  with _ -> raise Malformed
