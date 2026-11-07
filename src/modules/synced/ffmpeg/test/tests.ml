let () =
  Harness.main
    (Test_avutil.requirements @ Test_avcodec.requirements @ Test_av.requirements
   @ Test_avfilter.requirements @ Test_swresample.requirements
   @ Test_swscale.requirements @ Test_side_data.requirements
   @ Test_av_output_option.requirements @ Test_av_flush_encoder.requirements
   @ Test_av_codec_attr_avcc.requirements)
