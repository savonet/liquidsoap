
let _ =
  let s = Rtp.new_session Rtp.Recv "127.0.0.1" 8888 in
  let buffer = String.make (128*1024) 'x' in
  let ts = ref 0 in
    while true do
      Rtp.recv s buffer ts ;
      ignore (Unix.write Unix.stdout buffer 0 (1024*128)) ;
    done
