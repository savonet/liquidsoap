open Alsa

let () =
  Printf.printf "Using ALSA %s\n\n%!" (Alsa.get_version ());
  let seq = Sequencer.create "default" `Input in
  Sequencer.set_client_name seq "OCaml test client";
  let port =
    Sequencer.create_port seq "Test port"
      [Port_cap_write; Port_cap_subs_write]
      [Port_type_MIDI_generic]
  in
  Printf.printf "port: %d\n%!" port;
  Sequencer.subscribe_read_all seq port;
  try
    while true do
      match (Sequencer.input_event seq).ev_event with
        | Sequencer.Event.Note_on n ->
            Printf.printf "note on: %d at %d\n%!" n.note_note n.note_velocity
        | Sequencer.Event.Note_off n ->
            Printf.printf "note off: %d\n%!" n.note_note
        | Sequencer.Event.Controller c ->
            Printf.printf "controller: %d / %d\n%!" c.controller_value
              c.controller_param
        | Sequencer.Event.Program_change c ->
            Printf.printf "program change: %d / %d\n%!" c.controller_value
              c.controller_param
        | Sequencer.Event.Pitch_bend c ->
            Printf.printf "pitch bend: %d\n%!" c.controller_value
        | Sequencer.Event.Unhandled n ->
            Printf.printf "unhandled event: %d\n%!" n
        | _ -> Printf.printf "ignored event\n%!"
    done
  with e -> Printf.printf "Error: %s\n%!" (Printexc.to_string e)
