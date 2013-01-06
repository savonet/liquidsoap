let launched = ref false

let watched = ref []

let m = Mutex.create ()

let file_mtime file =
  (Unix.stat file).Unix.st_mtime

let rec watchdog () =
  let handler =
    Tutils.mutexify m (fun _ ->
      watched :=
        List.map
        (fun (file,mtime,f) ->
          let mtime' = file_mtime file in
          if mtime' <> mtime then f ();
          file,mtime',f
        ) !watched;
       [ watchdog () ])
  in
  { Duppy.Task.
    priority = Tutils.Maybe_blocking;
    events = [ `Delay 1. ];
    handler;
  }

let watch : File_watcher.watch = fun e file f ->
  if not !launched then
    begin
      launched := true;
      Duppy.Task.add Tutils.scheduler (watchdog ())
    end;
  if List.mem `Modify e then
    (Tutils.mutexify m (fun () ->
      watched := (file,file_mtime file,f) :: !watched;
      let unwatch =
        Tutils.mutexify m (fun () ->
          watched := List.filter (fun (fname,_,_) -> fname <> file) !watched
        )
      in
      unwatch)) ()
  else
    fun () -> ()
