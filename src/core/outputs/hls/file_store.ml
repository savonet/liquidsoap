type kind = [ `Playlist | `Segment of string | `Init of string ]

type t = {
  dir : string;
  temp_dir : string option;
  perm : int;
  dir_perm : int;
  reserved : string list;
  on_event : state:string -> path:string -> unit;
  tracked : (string, kind) Hashtbl.t;
  lock : Mutex.t;
}

let log = Log.make ["hls"; "files"]
let temp_prefix = ".hls-tmp-"

let create ~dir ~temp_dir ~perm ~dir_perm ~reserved ~on_event () =
  {
    dir;
    temp_dir;
    perm;
    dir_perm;
    reserved;
    on_event;
    tracked = Hashtbl.create 16;
    lock = Mutex.create ();
  }

let dir t = t.dir
let path t name = Filename.concat t.dir name
let locked t f = Mutex.protect t.lock f
let is_tracked t name = locked t (fun () -> Hashtbl.mem t.tracked name)
let tracked t = locked t (fun () -> List.of_seq (Hashtbl.to_seq t.tracked))
let track t name kind = locked t (fun () -> Hashtbl.replace t.tracked name kind)

let valid_name t name =
  name <> "" && Filename.is_relative name
  && (not (List.mem ".." (String.split_on_char '/' name)))
  && (not (List.mem name t.reserved))
  && not (is_tracked t name)

let temp_file t dir =
  let rec attempt n =
    let name =
      Filename.concat dir
        (Printf.sprintf "%s%d-%08x" temp_prefix (Unix.getpid ())
           (Random.bits ()))
    in
    match
      Unix.openfile name [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL] t.perm
    with
      | fd -> (name, fd)
      | exception Unix.Unix_error (Unix.EEXIST, _, _) when n > 0 ->
          attempt (n - 1)
  in
  attempt 10

let write_bytes fd bytes =
  Strings.iter
    (fun s offset length ->
      let rec write offset length =
        if length > 0 then (
          let n = Unix.write_substring fd s offset length in
          write (offset + n) (length - n))
      in
      write offset length)
    bytes

let write_temp t dir bytes =
  let name, fd = temp_file t dir in
  match
    Fun.protect
      ~finally:(fun () -> Unix.close fd)
      (fun () -> write_bytes fd bytes)
  with
    | () -> name
    | exception exn ->
        (try Unix.unlink name with _ -> ());
        raise exn

(* A rename across file systems fails: the bytes are then written again next
   to the final path, which is renamed. *)
let rename_into t temp final bytes =
  try Unix.rename temp final
  with Unix.Unix_error (Unix.EXDEV, _, _) -> (
    (try Unix.unlink temp with _ -> ());
    let local = write_temp t (Filename.dirname final) bytes in
    try Unix.rename local final
    with exn ->
      (try Unix.unlink local with _ -> ());
      raise exn)

let write t ~name kind bytes =
  let final = path t name in
  let temp = write_temp t (Option.value t.temp_dir ~default:t.dir) bytes in
  let existed =
    try
      Utils.mkdir ~perm:t.dir_perm (Filename.dirname final);
      let existed = Sys.file_exists final in
      rename_into t temp final bytes;
      existed
    with exn ->
      (try Unix.unlink temp with _ -> ());
      raise exn
  in
  track t name kind;
  let state = if existed then "updated" else "created" in
  fun () -> t.on_event ~state ~path:final

let write_now t ~name kind bytes = write t ~name kind bytes ()

let delete t name =
  if is_tracked t name then (
    let final = path t name in
    Fun.protect
      ~finally:(fun () -> locked t (fun () -> Hashtbl.remove t.tracked name))
      (fun () ->
        t.on_event ~state:"deleted" ~path:final;
        try Unix.unlink final
        with Unix.Unix_error (e, _, _) ->
          log#important "Could not delete %s: %s" final (Unix.error_message e)))
