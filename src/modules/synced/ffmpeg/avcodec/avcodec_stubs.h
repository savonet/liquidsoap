/* Services of the avcodec binding for the stubs of dependent libraries.

   Every function needs the OCaml runtime lock unless its comment says
   otherwise. "Raises" means the function may leave through an OCaml
   exception: the caller frees what it owns before the call. */

#ifndef OCAML_FFMPEG_AVCODEC_STUBS_H
#define OCAML_FFMPEG_AVCODEC_STUBS_H

#include <libavcodec/avcodec.h>

#include "avutil_stubs.h"

/* The native codec of a codec value. */
#define Codec_val(v) (*(const AVCodec **)Data_abstract_val(v))

/* A codec value. Raises a failure on a null pointer. */
value ocaml_avcodec_wrap_codec(const AVCodec *codec);

/* The native parameters a params value owns. */
#define CodecParameters_val(v) (*(AVCodecParameters **)Data_custom_val(v))

/* A params value owning a copy of [source]. Raises, a failure on a null
   pointer among others. */
value ocaml_avcodec_copy_parameters(const AVCodecParameters *source);

/* The native packet a packet value owns. */
#define Packet_val(v) (*(AVPacket **)Data_custom_val(v))

/* A packet value that takes ownership of [packet]. Raises a failure on a
   null pointer. */
value ocaml_avcodec_wrap_packet(AVPacket *packet);

/* Opens [context] for [codec], spec/avcodec.md §4.9: the thread count is
   automatic unless [options] says otherwise. [options] may be null; the
   entries FFmpeg did not use are left in it. Called with the lock held, it
   releases it around the open. Returns FFmpeg's code and raises nothing. */
int ocaml_avcodec_open(AVCodecContext *context, const AVCodec *codec,
                       AVDictionary **options);

/* Assigns the typed arguments of Audio.create_encoder to a context that is
   not opened. [_side_data] is a Frame_side_data.raw list. Needs no lock;
   returns FFmpeg's code and raises nothing. */
int ocaml_avcodec_set_audio_encoding(AVCodecContext *context,
                                     const AVChannelLayout *layout,
                                     int sample_rate,
                                     enum AVSampleFormat sample_format,
                                     AVRational time_base, value _side_data);

/* The same for Video.create_encoder. [frame_rate] has a zero numerator when
   none is given; [_hardware_context] is a Video.hardware_context option, of
   which the context takes a reference of its own. */
int ocaml_avcodec_set_video_encoding(AVCodecContext *context,
                                     enum AVPixelFormat pixel_format, int width,
                                     int height, AVRational time_base,
                                     AVRational frame_rate,
                                     value _hardware_context, value _side_data);

/* Gives a frame to an opened encoder, through its hardware frame pool when
   it has one; a null frame signals the end of the stream. Touches no OCaml
   state: it is called with the lock released. Returns FFmpeg's code. */
int ocaml_avcodec_encoder_send(AVCodecContext *context, const AVFrame *frame);

/* The codec identifier tables of the four families. These functions need
   no lock and raise nothing. */
const ocaml_ffmpeg_variant_table *ocaml_avcodec_audio_id_table(void);
const ocaml_ffmpeg_variant_table *ocaml_avcodec_video_id_table(void);
const ocaml_ffmpeg_variant_table *ocaml_avcodec_subtitle_id_table(void);
const ocaml_ffmpeg_variant_table *ocaml_avcodec_unknown_id_table(void);

#endif
