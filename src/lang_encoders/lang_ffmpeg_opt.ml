let () =
  Lang_ffmpeg.flag_qscale := Avcodec.flag_qscale;
  Lang_ffmpeg.qp2lambda := Avutil.qp2lambda
