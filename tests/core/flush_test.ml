(* This can be used to manually benchmark memory usage. Otherwise, it simply exists. *)

let () = exit 0

let _ =
  Stdlib.Lazy.force Builtins_settings.settings_module;
  Lang.eval ~cache:true ~typecheck:false ~stdlib:`Disabled
    {|
%include "../../src/libs/stdlib.liq"
enable_autocue_metadata()
|}

let () =
  Frame_settings.lazy_config_eval := true;
  Dtools.Log.conf_level#set 4;
  Dtools.Log.conf_stdout#set true;
  Dtools.Log.conf_file#set false;
  Dtools.Init.exec Dtools.Log.start;
  Tutils.start ();
  for _ = 0 to 10 do
    let r =
      Request.create ~cue_in_metadata:None ~cue_out_metadata:None "/tmp/bla.mp3"
    in
    ignore (Request.resolve r);
    Request.destroy r;
    Gc.compact ()
  done;
  Dtools.Init.exec Dtools.Log.stop;
  Tutils.shutdown 0
