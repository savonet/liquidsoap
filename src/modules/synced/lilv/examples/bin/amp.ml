open Lilv

let () =
  let w = World.create () in
  World.load_all w;
  let pp = World.plugins w in
  let uri = "http://lv2plug.in/plugins/eg-amp" in
  let p = Plugins.get_by_uri pp uri in
  Printf.printf "Loaded %s.\n%!" uri;
  let port_gain = Plugin.port_by_symbol p "gain" in
  let port_in = Plugin.port_by_symbol p "in" in
  let port_out = Plugin.port_by_symbol p "out" in
  let samples_length = 1024 in
  let samples =
    Bigarray.Array1.create Bigarray.Float32 Bigarray.c_layout samples_length
  in
  let i = Plugin.instantiate p 44100. in
  Plugin.Instance.connect_port_float i (Port.index port_gain) samples;
  Plugin.Instance.connect_port_float i (Port.index port_in) samples;
  Plugin.Instance.connect_port_float i (Port.index port_out) samples;
  Plugin.Instance.activate i;
  Printf.printf "Running.\n%!";
  Plugin.Instance.run i samples_length;
  Plugin.Instance.deactivate i;
  Printf.printf "Bye.\n%!";
  Gc.full_major ()
