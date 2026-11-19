(* Destroying a request while another thread resolves it: the resolution
   fails, and the temporary file it pushed is removed. *)

let m = Mutex.create ()
let c = Condition.create ()
let resolving = ref false
let destroyed = ref false
let tmp = Filename.temp_file "request_destroy" ".tmp"

let wait flag =
  Mutex.protect m (fun () ->
      while not !flag do
        Condition.wait c m
      done)

let set flag =
  Mutex.protect m (fun () ->
      flag := true;
      Condition.broadcast c)

let () =
  Plug.register Request.protocols "block" ~doc:""
    {
      Request.mode = Uri;
      static = (fun _ -> false);
      resolve =
        (fun _ ~log:_ _ ->
          set resolving;
          wait destroyed;
          Some (Request.indicator ~temporary:true tmp));
    };
  Plug.mark_complete ()

let () =
  let r =
    Request.create ~resolve_metadata:false ~cue_in_metadata:None
      ~cue_out_metadata:None "block:x"
  in
  let result = ref None in
  let th =
    Thread.create
      (fun () ->
        Fun.protect
          ~finally:(fun () -> set resolving)
          (fun () -> result := Some (Request.resolve ~timeout:10. r)))
      ()
  in
  wait resolving;
  Request.destroy r;
  set destroyed;
  Thread.join th;
  if !result <> Some `Failed then (
    print_endline "Resolution of a destroyed request did not fail";
    exit 1);
  if Sys.file_exists tmp then (
    print_endline "Temporary file of a destroyed request was not removed";
    exit 1)
