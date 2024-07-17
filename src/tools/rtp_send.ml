
let _ =
  let s = Rtp.new_session Rtp.Send "224.0.1.20" 8888 in
  let buffer = Mixer.Buffer.create () in
    Mixer.Buffer.set_already buffer Mixer.Buffer.size ;
    for i = 1 to 500 do
      Rtp.send s buffer
    done
