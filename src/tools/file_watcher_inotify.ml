type t = Inotify.wd

type event = Modify

let thread = ref (None : Thread.t option)
let fd = ref (None : Unix.file_descr option)
let handlers = ref []

let m = Mutex.create ()

let watchdog () =
  let fd = Utils.get_some !fd in
  while true do
    let _ = Unix.select [fd] [] [] (-1.) in
    let events = Inotify.read fd in
    List.iter
      (fun (wd,_,_,file) ->
        Printf.printf "Inotify: %s\n********************************\n%!" (Utils.get_some file);
        let f = List.assoc wd !handlers in
        f ()
      ) events
  done

let watch e file (f:unit -> unit) =
  Mutex.lock m;
  if !thread = None then
    (
      fd := Some (Inotify.init ());
      thread := Some (Thread.create watchdog ())
    );
  Mutex.unlock m;
  let fd = Utils.get_some !fd in
  match e with
  | Modify ->
    Mutex.lock m;
    let wd = Inotify.add_watch fd file [Inotify.S_Modify] in
    handlers := (wd,f) :: !handlers;
    Mutex.unlock m;
    wd

let unwatch wd =
  let fd = Utils.get_some !fd in
  Inotify.rm_watch fd wd;
  Mutex.lock m;
  handlers := List.remove_assoc wd !handlers;
  Mutex.unlock m
