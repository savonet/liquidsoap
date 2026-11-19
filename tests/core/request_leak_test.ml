(* The leak warning counts requests that are not destroyed: destroyed ones that
   are still referenced do not count, and collected ones are destroyed by their
   finaliser. It warns once per new high. *)

let warnings = ref []

let contains ~sub s =
  let n = String.length sub in
  let rec f i =
    i + n <= String.length s && (String.sub s i n = sub || f (i + 1))
  in
  f 0

let () =
  Dtools.Log.add_custom_log "test"
    {
      Dtools.Log.timestamp = false;
      exec =
        (fun msg ->
          if contains ~sub:"possible request leak" msg then
            warnings := msg :: !warnings);
    };
  (Dtools.Conf.as_int (Configure.conf#path ["request"; "leak_warning"]))#set 3

let create () =
  Request.create ~cue_in_metadata:None ~cue_out_metadata:None "/nonexistent"

let check ~expected =
  Dtools.Log.flush ();
  let expected = List.map (Printf.sprintf "currently %d RIDs") expected in
  if
    List.length !warnings <> List.length expected
    || not
         (List.for_all2
            (fun sub msg -> contains ~sub msg)
            expected (List.rev !warnings))
  then (
    Printf.printf "Expected warnings for %s, got:\n%s\n"
      (String.concat ", " expected)
      (String.concat "\n" (List.rev !warnings));
    exit 1)

let () =
  let destroyed =
    List.init 10 (fun _ ->
        let r = create () in
        Request.destroy r;
        r)
  in
  check ~expected:[];
  for _ = 1 to 5 do
    ignore (create ());
    ignore (create ());
    Gc.full_major ()
  done;
  check ~expected:[];
  let alive = List.init 3 (fun _ -> create ()) in
  check ~expected:[3];
  Request.destroy (List.hd alive);
  let alive = create () :: alive in
  check ~expected:[3];
  ignore (Sys.opaque_identity (destroyed, alive))
