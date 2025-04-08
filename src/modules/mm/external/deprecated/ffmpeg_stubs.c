/*
 * Copyright 2011 The Savonet Team
 *
 * This file is part of ocaml-mm.
 *
 * ocaml-mm is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-mm is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-mm; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * As a special exception to the GNU Library General Public License, you may
 * link, statically or dynamically, a "work that uses the Library" with a
 * publicly distributed version of the Library to produce an executable file
 * containing portions of the Library, and distribute that executable file under
 * terms of your choice, without any of the additional requirements listed in
 * clause 6 of the GNU Library General Public License. By "a publicly
 * distributed version of the Library", we mean either the unmodified Library as
 * distributed by The Savonet Team, or a modified version of the Library that is
 * distributed under the conditions defined in clause 3 of the GNU Library
 * General Public License. This exception does not however invalidate any other
 * reasons why the executable file might be covered by the GNU Library General
 * Public License.
 *
 */

/* Inspired of
 * http://dranger.com/ffmpeg/tutorial01.html
 * and

 */

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

#include <assert.h>

#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libswscale/swscale.h>

#include "image_c.h"

typedef struct {
  AVFormatContext *av_format_ctx;
  AVCodecContext *av_codec_ctx;
  AVCodec *av_codec;
  struct SwsContext *convert_ctx;
  int video_stream;
  AVFrame *av_frame;
  AVFrame *av_frame_rgb;
  uint8_t *buffer;
} ffmpeg_dec_t;

#define Dec_val(v) ((ffmpeg_dec_t *)v)

CAMLprim value caml_ffmpeg_init(value unit) {
  CAMLparam0();
  av_register_all();
  CAMLreturn(Val_unit);
}

/* TODO: add a finalizer!!!! */
CAMLprim value caml_ffmpeg_dec_openfile(value fname) {
  CAMLparam1(fname);
  ffmpeg_dec_t *ffd = malloc(sizeof(ffmpeg_dec_t));
  int i;
  int buflen;
  int width, height;

  /* Open the file */
  assert(av_open_input_file(&ffd->av_format_ctx, String_val(fname), NULL, 0,
                            NULL) == 0);
  /* Retrieve stream information */
  assert(av_find_stream_info(ffd->av_format_ctx) >= 0);

  ffd->video_stream = -1;
  /* Find a video stream */
  for (i = 0; i < ffd->av_format_ctx->nb_streams; i++)
    if (ffd->av_format_ctx->streams[i]->codec->codec_type == CODEC_TYPE_VIDEO) {
      ffd->video_stream = i;
      break;
    }
  assert(ffd->video_stream != -1);

  ffd->av_codec_ctx = ffd->av_format_ctx->streams[ffd->video_stream]->codec;
  /* Find a decoder */
  ffd->av_codec = avcodec_find_decoder(ffd->av_codec_ctx->codec_id);
  /* Is the codec supported? */
  assert(ffd->av_codec);
  /* Open the codec */
  assert(avcodec_open(ffd->av_codec_ctx, ffd->av_codec) >= 0);

  width = ffd->av_codec_ctx->width;
  height = ffd->av_codec_ctx->height;

  ffd->av_frame = avcodec_alloc_frame();
  ffd->av_frame_rgb = avcodec_alloc_frame();
  /* Allocate a suitable buffer */
  buflen = avpicture_get_size(PIX_FMT_RGBA, width, height);
  ffd->buffer = (uint8_t *)av_malloc(buflen * sizeof(uint8_t));
  /* Assign appropriate parts of buffer to image planes in av_frame_rgb */
  avpicture_fill((AVPicture *)ffd->av_frame_rgb, ffd->buffer, PIX_FMT_RGBA,
                 width, height);
  /* Init conversion context */
  ffd->convert_ctx =
      sws_getContext(width, height, ffd->av_codec_ctx->pix_fmt, width, height,
                     PIX_FMT_RGBA, SWS_BICUBIC, NULL, NULL, NULL);
  assert(ffd->convert_ctx);

  CAMLreturn((value)ffd);
}

CAMLprim value caml_ffmpeg_dec_dump_format(value _ffd, value _fname) {
  CAMLparam2(_ffd, _fname);
  ffmpeg_dec_t *ffd = Dec_val(_ffd);

  /* Dump info about the file on stderr */
  dump_format(ffd->av_format_ctx, 0, String_val(_fname), 0);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_ffmpeg_dec_set_target_size(value _ffd, value _w, value _h) {
  CAMLparam1(_ffd);
  ffmpeg_dec_t *ffd = Dec_val(_ffd);
  int w = Int_val(_w);
  int h = Int_val(_h);
  int width = ffd->av_codec_ctx->width;
  int height = ffd->av_codec_ctx->height;

  sws_freeContext(ffd->convert_ctx);
  ffd->convert_ctx =
      sws_getContext(width, height, ffd->av_codec_ctx->pix_fmt, w, h,
                     PIX_FMT_RGBA, SWS_BICUBIC, NULL, NULL, NULL);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_ffmpeg_dec_width(value ffd) {
  CAMLparam1(ffd);
  CAMLreturn(Val_int(Dec_val(ffd)->av_codec_ctx->width));
}

CAMLprim value caml_ffmpeg_dec_height(value ffd) {
  CAMLparam1(ffd);
  CAMLreturn(Val_int(Dec_val(ffd)->av_codec_ctx->height));
}

CAMLprim value caml_ffmpeg_dec_fps(value _ffd) {
  CAMLparam1(_ffd);
  ffmpeg_dec_t *ffd = Dec_val(_ffd);
  double n = (double)ffd->av_codec_ctx->time_base.num;
  double d = (double)ffd->av_codec_ctx->time_base.den;
  CAMLreturn(caml_copy_double(d / n));
}

CAMLprim value caml_ffmpeg_dec_read_frame(value _ffd, value _rgb) {
  CAMLparam2(_ffd, _rgb);
  CAMLlocal1(ans);
  frame rgb;
  ffmpeg_dec_t *ffd = Dec_val(_ffd);
  AVPacket packet;
  int frame_finished;
  int width = ffd->av_codec_ctx->width;
  int height = ffd->av_codec_ctx->height;
  int ansbuflen = width * height * 3;
  int j;

  frame_of_value(_rgb, &rgb);
  assert(rgb.width == width && rgb.height == height);

  caml_enter_blocking_section();
  while (av_read_frame(ffd->av_format_ctx, &packet) >= 0) {
    if (packet.stream_index == ffd->video_stream) {
      avcodec_decode_video(ffd->av_codec_ctx, ffd->av_frame, &frame_finished,
                           packet.data, packet.size);
      if (frame_finished) {
        sws_scale(ffd->convert_ctx, (const uint8_t *const *)ffd->av_frame->data,
                  ffd->av_frame->linesize, 0, height, ffd->av_frame_rgb->data,
                  ffd->av_frame_rgb->linesize);
        for (j = 0; j < height; j++)
          memcpy(rgb.data + j * width * 4,
                 ffd->av_frame_rgb->data[0] +
                     j * ffd->av_frame_rgb->linesize[0],
                 width * 4);
        caml_leave_blocking_section();
        CAMLreturn(Val_unit);
      }
    }
    /* Free the packet allocated by av_read_frame */
    av_free_packet(&packet);
  }
  caml_leave_blocking_section();

  caml_raise_constant(*caml_named_value("ffmpeg_exn_end_of_stream"));
}

/* TODO: finalizer!!!! */
CAMLprim value caml_ffmpeg_dec_close(value _ffd) {
  CAMLparam1(_ffd);
  ffmpeg_dec_t *ffd = Dec_val(_ffd);

  sws_freeContext(ffd->convert_ctx);
  av_free(ffd->buffer);
  av_free(ffd->av_frame_rgb);
  av_free(ffd->av_frame);
  free(ffd);

  CAMLreturn(Val_unit);
}

/* See
 * http://cekirdek.pardus.org.tr/~ismail/ffmpeg-docs/output-example_8c-source.html
 */

typedef struct {
  AVFormatContext *format_ctx;
  AVStream *video_stream;
  uint8_t *video_buffer;
  int video_buffer_size;
  AVFrame *frame;
  AVFrame *tmpframe;
  struct SwsContext *convert_ctx;
} ffmpeg_enc_t;

#define Enc_val(v) ((ffmpeg_enc_t *)v)

static AVFrame *alloc_picture(int pix_fmt, int width, int height) {
  AVFrame *picture;
  uint8_t *picture_buf;
  int size;

  picture = avcodec_alloc_frame();
  if (!picture)
    return NULL;
  size = avpicture_get_size(pix_fmt, width, height);
  picture_buf = malloc(size);
  if (!picture_buf) {
    av_free(picture);
    return NULL;
  }
  avpicture_fill((AVPicture *)picture, picture_buf, pix_fmt, width, height);
  return picture;
}

CAMLprim value caml_ffmpeg_enc_openfile(value _fname, value _fr, value _width,
                                        value _height, value _bitrate) {
  CAMLparam2(_fname, _fr);
  ffmpeg_enc_t *ffe = malloc(sizeof(ffmpeg_enc_t));
  AVOutputFormat *output_format;
  int frn = Int_val(Field(_fr, 0));
  int frd = Int_val(Field(_fr, 1));

  /* Guess the output format based on the extension */
  output_format = av_guess_format(NULL, String_val(_fname), NULL);
  if (!output_format)
    /* Fallback on mpeg */
    output_format = av_guess_format("mpeg", NULL, NULL);
  assert(output_format);

  // printf("Found format: %s\n", output_format->name);

  /* Allocate the output media context */
  ffe->format_ctx = avformat_alloc_context();
  assert(ffe->format_ctx);
  ffe->format_ctx->oformat = output_format;

  /* Add a video stream. */
  /* TODO: also allocate the audio stream if necessary */
  assert(output_format->video_codec != CODEC_ID_NONE);
  // ffe->video_stream = add_video_stream(ffe->format_ctx,
  // ffe->output_format->video_codec);
  ffe->video_stream = av_new_stream(ffe->format_ctx, 0);
  assert(ffe->video_stream);
  AVCodecContext *c = ffe->video_stream->codec;
  c->codec_id = output_format->video_codec;
  c->codec_type = CODEC_TYPE_VIDEO;
  c->bit_rate = Int_val(_bitrate);
  c->width = Int_val(_width); /* Resolution must be a power of two */
  c->height = Int_val(_height);
  /* TODO: in parameter */
  /* time base: this is the fundamental unit of time (in seconds) in terms of
     which frame timestamps are represented. for fixed-fps content, timebase
     should be 1/framerate and timestamp increments should be identically 1. */
  c->time_base.den = frn;
  c->time_base.num = frd;
  c->gop_size = 12; /* emit one intra frame every twelve frames at most */
  c->pix_fmt = PIX_FMT_YUV420P;
  /* just for testing, we also add B frames */
  if (c->codec_id == CODEC_ID_MPEG2VIDEO)
    c->max_b_frames = 2;
  /* needed to avoid using macroblocks in which some coeffs overflow this
     doesn't happen with normal video, it just happens here as the motion of
     the chroma plane doesn't match the luma plane */
  if (c->codec_id == CODEC_ID_MPEG1VIDEO)
    c->mb_decision = 2;
  /* some formats want stream headers to be separate */
  if (!strcmp(ffe->format_ctx->oformat->name, "mp4") ||
      !strcmp(ffe->format_ctx->oformat->name, "mov") ||
      !strcmp(ffe->format_ctx->oformat->name, "3gp"))
    c->flags |= CODEC_FLAG_GLOBAL_HEADER;

  /* Set the parameters */
  assert(av_set_parameters(ffe->format_ctx, NULL) >= 0);

  /* Display what we have so far on stderr */
  // dump_format(ffe->format_ctx, 0, String_val(_fname), 1);

  /* Now that all the parameters are set, we can open the audio and video codecs
   * and allocate the necessary encode buffers */
  c = ffe->video_stream->codec;
  AVCodec *codec = avcodec_find_encoder(c->codec_id);
  assert(codec);
  assert(avcodec_open(c, codec) >= 0);
  ffe->video_buffer = NULL;
  ffe->video_buffer_size = 0;
  if (!(ffe->format_ctx->oformat->flags & AVFMT_RAWPICTURE)) {
    ffe->video_buffer_size = 200000;
    ffe->video_buffer = av_malloc(ffe->video_buffer_size);
  }
  /* allocate the encoded raw picture */
  ffe->frame = alloc_picture(c->pix_fmt, c->width, c->height);
  assert(ffe->frame);
  /* if the output format is not YUV420P, then a temporary YUV420P picture is
     needed too. It is then converted to the required output format */
  ffe->tmpframe = NULL;
  if (c->pix_fmt != PIX_FMT_RGBA)
    ffe->tmpframe = alloc_picture(PIX_FMT_RGBA, c->width, c->height);

  /* Prepare the conversion context */
  ffe->convert_ctx =
      sws_getContext(c->width, c->height, PIX_FMT_RGBA, c->width, c->height,
                     c->pix_fmt, SWS_BICUBIC, NULL, NULL, NULL);

  /* open the output file, if needed */
  if (!(output_format->flags & AVFMT_NOFILE))
    assert(url_fopen(&ffe->format_ctx->pb, String_val(_fname), URL_WRONLY) >=
           0);

  /* write the stream header, if any */
  av_write_header(ffe->format_ctx);

  CAMLreturn((value)ffe);
}

CAMLprim value caml_ffmpeg_enc_dump_format(value _ffe, value _fname) {
  CAMLparam2(_ffe, _fname);
  ffmpeg_enc_t *ffe = Enc_val(_ffe);

  /* Dump info about the file on stderr */
  dump_format(ffe->format_ctx, 0, String_val(_fname), 1);

  CAMLreturn(Val_unit);
}

static void fill_picture(AVFrame *pict, frame *rgb) {
  int j;

  for (j = 0; j < rgb->height; j++)
    memcpy(pict->data[0] + j * pict->linesize[0],
           rgb->data + j * 4 * rgb->width, 4 * rgb->width);
}

CAMLprim value caml_ffmpeg_enc_write_frame(value _ffe, value _f) {
  CAMLparam2(_ffe, _f);
  frame rgb;
  frame_of_value(_f, &rgb);
  ffmpeg_enc_t *ffe = Enc_val(_ffe);
  AVCodecContext *c = ffe->video_stream->codec;
  int out_size;

  assert(rgb.width == c->width && rgb.height == c->height);

  caml_enter_blocking_section();

  /* We have to convert the frame to the right format. */
  if (c->pix_fmt != PIX_FMT_RGBA) {
    fill_picture(ffe->tmpframe, &rgb);
    sws_scale(ffe->convert_ctx, (const uint8_t *const *)ffe->tmpframe->data,
              ffe->tmpframe->linesize, 0, c->height, ffe->frame->data,
              ffe->frame->linesize);
  } else
    fill_picture(ffe->frame, &rgb);

  if (ffe->format_ctx->oformat->flags & AVFMT_RAWPICTURE) {
    /* raw video case. The API will change slightly in the near futur for
       that */
    AVPacket pkt;
    av_init_packet(&pkt);

    pkt.flags |= PKT_FLAG_KEY;
    pkt.stream_index = ffe->video_stream->index;
    pkt.data = (uint8_t *)ffe->frame;
    pkt.size = sizeof(AVPicture);

    assert(av_write_frame(ffe->format_ctx, &pkt) == 0);
  } else {
    /* encode the image */
    out_size = avcodec_encode_video(c, ffe->video_buffer,
                                    ffe->video_buffer_size, ffe->frame);
    /* if zero size, it means the image was buffered */
    if (out_size > 0) {
      AVPacket pkt;
      av_init_packet(&pkt);

      pkt.pts = av_rescale_q(c->coded_frame->pts, c->time_base,
                             ffe->video_stream->time_base);
      if (c->coded_frame->key_frame)
        pkt.flags |= PKT_FLAG_KEY;
      pkt.stream_index = ffe->video_stream->index;
      pkt.data = ffe->video_buffer;
      pkt.size = out_size;

      /* write the compressed frame in the media file */
      assert(av_write_frame(ffe->format_ctx, &pkt) == 0);
    }
  }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_ffmpeg_enc_close(value _ffe) {
  CAMLparam1(_ffe);
  ffmpeg_enc_t *ffe = Enc_val(_ffe);
  int i;

  /* Close codec */
  avcodec_close(ffe->video_stream->codec);
  av_free(ffe->frame->data[0]);
  av_free(ffe->frame);
  if (ffe->tmpframe) {
    av_free(ffe->tmpframe->data[0]);
    av_free(ffe->tmpframe);
  }
  av_free(ffe->video_buffer);

  /* write the trailer, if any */
  av_write_trailer(ffe->format_ctx);

  /* free the streams */
  for (i = 0; i < ffe->format_ctx->nb_streams; i++) {
    av_freep(&ffe->format_ctx->streams[i]->codec);
    av_freep(&ffe->format_ctx->streams[i]);
  }

  /* close the output file */
  if (!(ffe->format_ctx->oformat->flags & AVFMT_NOFILE))
    url_fclose(ffe->format_ctx->pb);

  /* free the stream */
  av_free(ffe->format_ctx);

  free(ffe);

  CAMLreturn(Val_unit);
}

/* TODO: finalizer with sws_freeContext */
/* TODO: handle pixel coding conversions too */
/*
CAMLprim caml_sws_create(value src, value tgt, value th)
{
  CAMLparam0();
  struct SwsContext *swsc;

  swsc = sws_getContext(Int_val(Field(src,0)), Int_val(Field(src,1)),
PIX_FMT_RGBA, Int_val(Field(src,0)), Int_val(Field(src,1)), PIX_FMT_RGBA,
SWS_BICUBIC, NULL, NULL, NULL);

  CAMLreturn((value)swsc);
}

CAMLprim caml_sws_scale_to(value _swsc, value _src, value _dst)
{
  CAMLparam2(_src, _tgt);
  frame src;
  frame dst;
  struct SwsContext *swsc = (struct SwsContext*)_swsc;

  frame_of_val(_src, &src);
  frame_of_val(_tgt, &tgt);

  caml_enter_blocking_section();
  // The coding of images is weird
  sws_scale(swsc, (const uint8_t * const*)src.data, src.width*4, 0, src.height,
dst.data, dst.width*4); caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}
*/
