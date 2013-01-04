type t = unit

type event = Modify

let thread = ref (None : Thread.t option)

let m = Mutex.create ()

let watched = ref []

let file_mtime file =
  (Unix.stat file).Unix.st_mtime

let watchdog () =
  while true do
    Unix.sleep 1;
    Mutex.lock m;
    watched :=
      List.map
      (fun (file,mtime,f) ->
        let mtime' = file_mtime file in
        if mtime' <> mtime then f ();
        file,mtime',f
      ) !watched;
    Mutex.unlock m
  done

let watch e file f =
  if !thread = None then thread := Some (Thread.create watchdog ());
  match e with
  | Modify ->
    Mutex.lock m;
    watched := (file,file_mtime file,f) :: !watched;
    Mutex.unlock m

(* TODO *)
let unwatch _ = ()
