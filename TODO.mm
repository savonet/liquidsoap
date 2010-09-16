* Check AFrame.to_s16le for non stereo uses
* Check that all ringbuffers that need it are thread safe (cf LiqMM)
* Access pixels as ints instead of color by color (start with get_pixel and set_pixel)
* Put formats in video_converter into ocaml-mm
* Use ocaml-mm for wav decoder / encoder
* Restore video_convert in ogg_decoder.ml (beware of the assert false)
* Bilinear scaling of image does not seem to work
* Integrate sdl_utils.ml into ocaml-mm
* Midi rate is the same as audio rate for now. Is this ok?
* Restore chord.ml and dssi
* Check MIDI
