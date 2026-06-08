open Ladspa

let p =
  try Plugin.load "/usr/lib/ladspa/sc4_1882x.so"
  with Plugin.Not_a_plugin ->
    print_endline "Could not load plugin, exiting.";
    exit 0

let d = Descriptor.descriptor p 0

let print_port n =
  Printf.printf " %02d. %s: %s %s\n%!" n (Descriptor.port_name d n)
    (if Descriptor.port_is_input d n then "input" else "output")
    (if Descriptor.port_is_control d n then "control" else "audio")

let () =
  Printf.printf "LADSPA %s\n\n%!" (Ladspa.version ());
  Printf.printf "Using plugin %s (%s) by %s, %s.\n\n%!" (Descriptor.label d)
    (Descriptor.name d) (Descriptor.maker d)
    (match Descriptor.copyright d with Some c -> c | None -> "");
  Printf.printf "Found %d descriptors.\n%!"
    (Array.length (Descriptor.descriptors p));
  Printf.printf "Found %d ports:\n%!" (Descriptor.port_count d);
  for i = 0 to Descriptor.port_count d - 1 do
    print_port i
  done

let samples_len = 1024

let samples =
  Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout samples_len

let inst = Descriptor.instantiate d 44100

let () =
  let c = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 1 in
  Descriptor.set_control_port inst 0 0.;
  Descriptor.set_control_port inst 1 0.;
  Descriptor.set_control_port inst 2 0.;
  Descriptor.set_control_port inst 3 0.;
  Descriptor.set_control_port inst 4 0.;
  Descriptor.set_control_port inst 5 0.;
  Descriptor.set_control_port inst 6 0.;
  Descriptor.connect_port inst 7 c;
  Descriptor.connect_port inst 8 c;
  Descriptor.connect_port inst 9 samples;
  Descriptor.connect_port inst 10 samples;
  Descriptor.connect_port inst 11 samples;
  Descriptor.connect_port inst 12 samples;
  Descriptor.activate inst;
  Descriptor.run inst samples_len;
  Descriptor.deactivate inst

let () =
  Gc.full_major ();
  Plugin.unload p;
  Gc.full_major ()
