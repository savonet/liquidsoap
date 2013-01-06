type event = [`Modify]

let fd = ref (None : Unix.file_descr option)
let handlers = ref []

let m = Mutex.create ()

let rec watchdog () =
  let fd = Utils.get_some !fd in
  let handler =
    Tutils.mutexify m (fun _ ->
      let events = Inotify.read fd in
      List.iter
        (fun (wd,_,_,file) ->
          let f = List.assoc wd !handlers in
          f ()
        ) events);
    [ watchdog () ]
  in
  { Duppy.Task.
    priority = Tutils.Maybe_blocking;
    events = [ `Read fd ];
    handler;
  }

let watch e file (f:unit -> unit) =
  if !fd = None then
   begin
      fd := Some (Inotify.init ());
      Duppy.Task.add Tutils.scheduler (watchdog ())
   end;
  let fd = Utils.get_some !fd in
  match e with
    | `Modify ->
      (Tutils.mutexify m (fun () ->
        let wd = Inotify.add_watch fd file [Inotify.S_Modify] in
        handlers := (wd,f) :: !handlers;
        let unwatch =
          Tutils.mutexify m (fun () ->
            let fd = Utils.get_some !fd in
            Inotify.rm_watch fd wd;
            handlers := List.remove_assoc wd !handlers)
        in
        unwatch)) ()

let () =
  Configure.watch.Configure.register <- watch
