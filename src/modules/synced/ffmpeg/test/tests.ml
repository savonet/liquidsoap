let () =
  Harness.main
    (Test_avutil.requirements @ Test_avcodec.requirements @ Test_av.requirements
   @ Test_avfilter.requirements @ Test_swresample.requirements
   @ Test_swscale.requirements)
