let basic_token headers =
  match List.assoc_opt "authorization" headers with
    | None -> None
    | Some value -> (
        match
          List.filter (( <> ) "")
            (Re.Pcre.split ~rex:(Re.Pcre.regexp "[ \t]+") value)
        with
          | "Basic" :: token :: _ -> (
              match Lang_string.decode64 token with
                | decoded -> Some decoded
                | exception _ -> None)
          | _ -> None)

let credentials ~headers ~query =
  match basic_token headers with
    | Some decoded -> (
        match Liq_http.parse_auth decoded with
          | { Liq_http.user; password } -> Some (user, password)
          | exception Not_found -> Some (decoded, ""))
    | None -> (
        match List.assoc_opt "pass" query with
          | Some password ->
              Some
                ( Option.value ~default:"" (List.assoc_opt "user" query),
                  password )
          | None -> None)

let unauthorized = function
  | `No_credentials ->
      Harbor.challenge_page 401 "Unauthorized" "No login / password supplied."
  | `Wrong_credentials ->
      Harbor.challenge_page 401 "Unauthorized" "Wrong Authentication data"

let mount_taken =
  Harbor.challenge_page 403 "Mountpoint already taken" "Mountpoint in use"

type refusal = {
  status : int;
  reason : string;
  headers : (string * string) list;
}

let header_line (name, value) = name ^ ": " ^ value ^ "\r\n"

let refusal_reply ~protocol { status; reason; headers } =
  Printf.sprintf "HTTP/%s %d %s\r\n%sContent-Length: 0\r\n\r\n" protocol status
    reason
    (String.concat "" (List.map header_line headers))

let forbidden = { status = 403; reason = "Forbidden"; headers = [] }

let unavailable ~protocol =
  refusal_reply ~protocol
    { status = 503; reason = "Service Unavailable"; headers = [] }

let is_token name =
  name <> ""
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
         | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^'
         | '_' | '`' | '|' | '~' ->
             true
         | _ -> false)
       name

let has_line_break = String.exists (fun c -> c = '\r' || c = '\n')

let header_error (name, value) =
  if not (is_token name) then Some "Header name must be an HTTP token."
  else if has_line_break value then
    Some "Header value must not contain a line break."
  else None

let refusal_error { status; reason; headers } =
  if status < 300 || status > 599 then
    Some "Refusal status must be between 300 and 599."
  else if has_line_break reason then
    Some "Refusal reason must not contain a line break."
  else List.find_map header_error headers

let response_head ~protocol ~mime ~metaint headers =
  String.concat ""
    ([
       "HTTP/" ^ protocol ^ " 200 OK\r\n";
       header_line ("Content-type", mime);
       header_line ("Connection", "close");
     ]
    @ (match metaint with
      | Some metaint -> [header_line ("icy-metaint", string_of_int metaint)]
      | None -> [])
    @ List.map header_line headers
    @ ["\r\n"])
