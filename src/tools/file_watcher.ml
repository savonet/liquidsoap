type t = unit

type event = Modify

let launched = ref false

let watched = ref []

let file_mtime file =
  (Unix.stat file).Unix.st_mtime

let rec watchdog () =
  let handler _ =
    watched :=
      List.map
      (fun (file,mtime,f) ->
        let mtime' = file_mtime file in
        if mtime' <> mtime then f ();
        file,mtime',f
      ) !watched;
     [ watchdog () ]
  in
  { Duppy.Task.
    priority = Tutils.Maybe_blocking;
    events = [ `Delay 1. ];
    handler;
  }

let watch e file f =
  if not !launched then
    (
      launched := true;
      Duppy.Task.add Tutils.scheduler (watchdog ())
    );
  match e with
  | Modify ->
    watched := (file,file_mtime file,f) :: !watched

(* TODO *)
let unwatch _ = ()
