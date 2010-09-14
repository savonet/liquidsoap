* Check AFrame.to_s16le for non stereo uses
* Check that all ringbuffers that need it are thread safe (cf LiqMM)
* Do we really need stride in images?
* Access pixels as ints instead of color by color (start with get_pixel and set_pixel)
* Put formats in video_converter into ocaml-mm
* Restore all effects in video_effect.ml
* Use ocaml-mm for wav decoder / encoder
* Restore theora and diract encoder
* Restore video_convert in ogg_decoder.ml (beware of the assert false)
* Grep LIQ_BIG_ENDIAN in ocaml-mm and fix it
* Bilinear scaling of image does not seem to work
* Integrate sdl_utils.ml into ocaml-mm
