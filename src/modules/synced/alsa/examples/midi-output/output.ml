open Alsa

let () =
  Printf.printf "Using ALSA %s\n\n%!" (Alsa.get_version ());
  let seq = Sequencer.create "default" `Output in
  Sequencer.set_client_name seq "OCaml test client";
  let port =
    Sequencer.create_port seq "Test port"
      [Port_cap_read; Port_cap_subs_read]
      [Port_type_application]
  in
  Printf.printf "port: %d\n\n%!" port;
  Sequencer.subscribe_write_all seq port;
  for i = 0 to 40 do
    Printf.printf "note: %d\n%!" i;
    Sequencer.output_event seq
      (Sequencer.Event.Note_on
         {
           Sequencer.Event.note_channel = 0;
           note_note = i;
           note_velocity = 127;
           note_off_velocity = 127;
           note_duration = 1000;
         });
    Unix.sleepf 0.1;
    Sequencer.output_event seq
      (Sequencer.Event.Note_on
         {
           Sequencer.Event.note_channel = 0;
           note_note = i;
           note_velocity = 0;
           note_off_velocity = 127;
           note_duration = 1000;
         })
  done
