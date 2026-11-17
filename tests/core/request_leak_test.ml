(* The leak warning counts requests that are not destroyed, including destroyed
   ones that are still referenced. *)

let warnings = ref 0

let () =
  Dtools.Log.add_custom_log "test"
    {
      Dtools.Log.timestamp = false;
      exec =
        (fun msg ->
          if String.ends_with ~suffix:"empty/unavailable requests." msg then
            incr warnings);
    };
  (Dtools.Conf.as_int (Configure.conf#path ["request"; "leak_warning"]))#set 3

let create () =
  Request.create ~cue_in_metadata:None ~cue_out_metadata:None "/nonexistent"

let check ~expected =
  Dtools.Log.flush ();
  if !warnings <> expected then (
    Printf.printf "Expected %d leak warnings, got %d\n" expected !warnings;
    exit 1)

let () =
  let destroyed =
    List.init 10 (fun _ ->
        let r = create () in
        Request.destroy r;
        r)
  in
  check ~expected:0;
  let alive = List.init 3 (fun _ -> create ()) in
  check ~expected:1;
  ignore (Sys.opaque_identity (destroyed, alive))
