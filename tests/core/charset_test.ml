let () = Curl.global_init Curl.CURLINIT_GLOBALALL

let writer accum data =
  Buffer.add_string accum data;
  String.length data

let fetch url =
  let result = Buffer.create 16384 in
  let connection = Curl.init () in
  Curl.set_writefunction connection (writer result);
  Curl.set_followlocation connection true;
  Curl.set_url connection url;
  Curl.perform connection;
  let code = Curl.get_responsecode connection in
  Curl.cleanup connection;
  match code with
    | 404 -> None
    | 200 -> Some (Buffer.contents result)
    | _ -> assert false

let base_url =
  "https://raw.githubusercontent.com/yoriyuki/Camomile/master/camomile-test/data/enc"

let to_string : Charset.t -> string = function
  | `ISO_8859_1 -> "ISO-8859-1"
  | `ISO_8859_10 -> "ISO-8859-10"
  | `ISO_8859_11 -> "ISO-8859-11"
  | `ISO_8859_13 -> "ISO-8859-13"
  | `ISO_8859_14 -> "ISO-8859-14"
  | `ISO_8859_15 -> "ISO-8859-15"
  | `ISO_8859_16 -> "ISO-8859-16"
  | `ISO_8859_2 -> "ISO-8859-2"
  | `ISO_8859_3 -> "ISO-8859-3"
  | `ISO_8859_4 -> "ISO-8859-4"
  | `ISO_8859_5 -> "ISO-8859-5"
  | `ISO_8859_6 -> "ISO-8859-6"
  | `ISO_8859_7 -> "ISO-8859-7"
  | `ISO_8859_8 -> "ISO-8859-8"
  | `ISO_8859_9 -> "ISO-8859-9"
  | `KOI8_R -> "KOI8-R"
  | `KOI8_U -> "KOI8-U"
  | `UTF_7 -> "UTF-7"
  | `UTF_8 -> "UTF-8"
  | `UTF_16 -> "UTF-16"
  | `UTF_16BE -> "UTF-16%23BE"
  | `UTF_16LE -> "UTF-16%23LE"

let encoded_file enc =
  let enc = to_string enc in
  [%string "%{base_url}/%{enc}"]

let utf8_file enc =
  let enc = to_string enc in
  [%string "%{base_url}/%{enc}..UTF8"]

let warning s =
  if Sys.getenv_opt "GITHUB_ACTIONS" <> None then
    Printf.printf "::warning file=%s,title=%s::\n%!" __FILE__ s;
  Printf.printf "⚠️  Warning: %s! ⚠️\n%!" s

let () =
  Printf.printf "Running charset conversion test suite with the %s\n%!"
    Charset.description;
  List.iter
    (fun enc ->
      let enc_str = Charset.to_string enc in
      match (fetch (encoded_file enc), fetch (utf8_file enc)) with
        | Some encoded, Some utf8 ->
            if
              List.mem enc Charset.can_decode
              && List.mem `UTF_8 Charset.can_encode
            then (
              Printf.printf "%s\n%!"
                [%string "Testing conversion from %{enc_str} to UTF8"];
              assert (Charset.convert ~source:enc ~target:`UTF_8 encoded = utf8);
              Printf.printf "%s\n%!"
                [%string "Testing conversion from %{enc_str} to default UTF8"];
              assert (Charset.convert ~source:enc encoded = utf8));
            if
              List.mem enc Charset.can_detect
              && List.mem `UTF_8 Charset.can_encode
            then (
              Printf.printf "%s\n%!"
                [%string
                  "Testing auto-detect conversion from %{enc_str} to UTF8"];
              if Charset.convert ~target:`UTF_8 encoded <> utf8 then
                warning
                  [%string
                    "Auto-detect conversion from %{enc_str} to UTF8 failed"];
              Printf.printf "%s\n%!"
                [%string
                  "Testing auto-detect conversion from %{enc_str} to default \
                   UTF8"];
              if Charset.convert encoded <> utf8 then
                warning
                  [%string
                    "Auto-detect conversion from %{enc_str} to default UTF8 \
                     failed"]);
            if
              List.mem `UTF_8 Charset.can_decode
              && List.mem enc Charset.can_encode
            then (
              Printf.printf "%s\n%!"
                [%string "Testing conversion from UTF8 to %{enc_str}"];
              assert (Charset.convert ~source:`UTF_8 ~target:enc utf8 = encoded));
            if
              List.mem `UTF_8 Charset.can_detect
              && List.mem enc Charset.can_encode
            then (
              Printf.printf "%s\n%!"
                [%string
                  "Testing auto-detect conversion from UTF8 to %{enc_str}"];
              if Charset.convert ~target:enc utf8 <> encoded then
                warning
                  [%string
                    "Auto-detect conversion from UTF8 to %{enc_str} failed"])
        | _ -> ())
    Charset.all_encodings
