(* Decoders report sizes while other threads read the dimensions: every
   reader must see the same size, and the configuration must agree with it. *)

let reporters = 4
let readers = 4

let () =
  Frame_settings.lazy_config_eval := true;
  Frame_settings.conf_video_detect_dimensions#set true;
  let go = Atomic.make false in
  let force (width, height) =
    (Lazy.Mutexed.force width, Lazy.Mutexed.force height)
  in
  let spawn f =
    Domain.spawn (fun () ->
        while not (Atomic.get go) do
          Domain.cpu_relax ()
        done;
        force (f ()))
  in
  let reporter n () =
    let ideal_size =
      { Frame.width = 100 + n; height = 200 + n; source = "test" }
    in
    Frame.video_dimensions ~ideal_size ()
  in
  let domains =
    List.init reporters (fun n -> spawn (reporter n))
    @ List.init readers (fun _ -> spawn (fun () -> Frame.video_dimensions ()))
  in
  Atomic.set go true;
  let seen = List.map Domain.join domains in
  let first = List.hd seen in
  assert (List.for_all (( = ) first) seen);
  let configured =
    (Frame_settings.conf_video_width#get, Frame_settings.conf_video_height#get)
  in
  if configured <> first then (
    Printf.printf "Read %dx%d, configured %dx%d\n" (fst first) (snd first)
      (fst configured) (snd configured);
    exit 1)
