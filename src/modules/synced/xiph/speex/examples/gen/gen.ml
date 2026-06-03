let executable name modules libraries =
  Printf.printf "(executable\n (name %s)\n (modules %s)\n (libraries %s))\n\n"
    name modules
    (String.concat " " libraries)

let () =
  if Has_speex.available then begin
    executable "speex2wav" "speex2wav" ["speex"; "speex.decoder"; "ogg.decoder"];
    executable "wav2speex" "wav2speex" ["speex"];
    print_string
      {|(rule
 (alias xiph_citest)
 (package speex)
 (deps
  (:speex test.ogg)
  (:wav2speex ./wav2speex.exe)
  (:speex2wav ./speex2wav.exe))
 (action
  (progn
   (run %{speex2wav} %{speex} decoded.wav)
   (run %{wav2speex} decoded.wav encoded.ogg))))
|}
  end
