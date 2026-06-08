open Ladspa

let p = Plugin.load "/usr/lib/ladspa/amp.so"
let d = Descriptor.descriptor p 0

let print_port n =
  Printf.printf " * %s: %s %s\n%!" (Descriptor.port_name d n)
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
  Descriptor.set_control_port inst 0 0.5;
  Descriptor.connect_port inst 1 samples;
  Descriptor.connect_port inst 2 samples;
  Descriptor.activate inst;
  Descriptor.run inst samples_len;
  Descriptor.deactivate inst

let () =
  Gc.full_major ();
  Plugin.unload p;
  Gc.full_major ()
