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

let header_line (name, value) = name ^ ": " ^ value ^ "\r\n"

let forbidden =
  { Harbor_refusal.status = 403; reason = "Forbidden"; headers = [] }

let unavailable ~protocol =
  Harbor_refusal.reply ~protocol
    { status = 503; reason = "Service Unavailable"; headers = [] }

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
