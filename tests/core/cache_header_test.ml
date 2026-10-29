(* A cache file is unmarshalled only when its header matches this build and
   the payload's hash matches; every other file is a miss. *)

module Cache = Liquidsoap_lang_cache.Cache

let () = ignore Lang.unit
let dir = Filename.temp_dir "liq-cache-test" ""
let () = Unix.putenv "LIQ_CACHE_USER_DIR" dir
let name = "test.liq-cache"
let path = Filename.concat dir name
let value = List.init 1000 (fun n -> (n, string_of_int n))

let retrieve kind : (int * string) list option =
  Cache.retrieve ~dirtype:`User ~kind name

let read () =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let write s =
  let oc = open_out_bin path in
  output_string oc s;
  close_out oc

let with_byte s pos f =
  let b = Bytes.of_string s in
  Bytes.set b pos (Char.chr (f (Char.code (Bytes.get b pos)) land 0xff));
  Bytes.to_string b

let () =
  Cache.store ~dirtype:`User ~kind:`Term name value;
  let stored = read () in
  assert (retrieve `Term = Some value);
  assert (retrieve `Table = None);
  let header = 36 in
  write (with_byte stored (header + 100) (fun c -> c lxor 1));
  assert (retrieve `Term = None);
  write (with_byte stored 12 (fun c -> c lxor 1));
  assert (retrieve `Term = None);
  write (with_byte stored 0 (fun c -> c lxor 1));
  assert (retrieve `Term = None);
  write (String.sub stored 0 (String.length stored - 1));
  assert (retrieve `Term = None);
  write (String.sub stored 0 10);
  assert (retrieve `Term = None);
  write (Marshal.to_string value [Marshal.Closures]);
  assert (retrieve `Term = None);
  write stored;
  assert (retrieve `Term = Some value);
  Cache.store ~dirtype:`User ~kind:`Term name value;
  assert (read () = stored);
  Sys.remove path;
  Unix.rmdir dir;
  print_endline "cache header checks passed"
