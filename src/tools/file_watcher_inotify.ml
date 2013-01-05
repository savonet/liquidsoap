type t = Inotify.wd

type event = Modify

let fd = ref (None : Unix.file_descr option)
let handlers = ref []

let rec watchdog () =
  let fd = Utils.get_some !fd in
  let handler _ =
    let events = Inotify.read fd in
    List.iter
      (fun (wd,_,_,file) ->
        let f = List.assoc wd !handlers in
        f ()
      ) events;
    [ watchdog () ]
  in
  { Duppy.Task.
    priority = Tutils.Maybe_blocking;
    events = [ `Read fd ];
    handler;
  }

let watch e file (f:unit -> unit) =
  if !fd = None then
    (
      fd := Some (Inotify.init ());
      Duppy.Task.add Tutils.scheduler (watchdog ())
    );
  let fd = Utils.get_some !fd in
  match e with
  | Modify ->
    let wd = Inotify.add_watch fd file [Inotify.S_Modify] in
    handlers := (wd,f) :: !handlers;
    wd

let unwatch wd =
  let fd = Utils.get_some !fd in
  Inotify.rm_watch fd wd;
  handlers := List.remove_assoc wd !handlers
