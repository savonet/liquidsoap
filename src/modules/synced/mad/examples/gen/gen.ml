let () =
  if Has_mad.available then
    print_string
      {|(executable
 (name decode_msg)
 (modules decode_msg)
 (libraries mad))

(executable
 (name mp32wav)
 (modules mp32wav)
 (libraries unix mad))

(executable
 (name mp3info)
 (modules mp3info)
 (libraries mad))

(executable
 (name mp3stream2wav)
 (modules mp3stream2wav)
 (libraries unix mad))

|}
