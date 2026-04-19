(** Guess the mime-type of a file. *)

module String = struct
  include String

  let contains_at offset ~substring s =
    let n = String.length substring in
    if String.length s < offset + n then false
    else String.sub s offset n = substring
end

let prefixes =
  [
    ("ID3", "audio/mpeg");
    ("OggS", "audio/ogg");
    ("%PDF-", "application/pdf");
    ("\137PNG\013\010\026\010", "image/png");
  ]

let advanced =
  let wav s =
    String.starts_with ~prefix:"RIFF" s
    && String.contains_at 8 ~substring:"WAVEfmt " s
  in
  let avi s =
    String.starts_with ~prefix:"RIFF" s
    && String.contains_at 8 ~substring:"AVI " s
  in
  [(wav, "audio/wav"); (avi, "video/x-msvideo")]

let of_string s =
  let ans = ref "" in
  try
    List.iter
      (fun (f, mime) ->
        if f s then (
          ans := mime;
          raise Exit))
      advanced;
    List.iter
      (fun (prefix, mime) ->
        if String.starts_with ~prefix s then (
          ans := mime;
          raise Exit))
      prefixes;
    raise Not_found
  with Exit -> !ans

let of_file fname =
  let len = 16 in
  let buf = Bytes.create len in
  let ic = open_in fname in
  let n = input ic buf 0 len in
  let buf = if n = len then buf else Bytes.sub buf 0 n in
  let s = Bytes.unsafe_to_string buf in
  of_string s
