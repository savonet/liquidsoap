/* Services of the av binding for the stubs of dependent libraries.

   Every function needs the OCaml runtime lock unless its comment says
   otherwise. "Raises" means the function may leave through an OCaml
   exception: the caller frees what it owns before the call. */

#ifndef OCAML_FFMPEG_AV_STUBS_H
#define OCAML_FFMPEG_AV_STUBS_H

#include <libavformat/avformat.h>

#include "avcodec_stubs.h"

/* The format context of a container value. Raises the closed and the
   failed errors of spec/binding-contract.md §5.3. */
AVFormatContext *ocaml_av_format_context(value _container);

/* Takes the guard of a container value exclusively; returns 0 when it is
   taken already. Raises nothing. */
int ocaml_av_try_guard(value _container);

/* Releases what ocaml_av_try_guard took. Raises nothing. */
void ocaml_av_release_guard(value _container);

/* The native format of a format value. */
#define InputFormat_val(v) (*(const AVInputFormat **)Data_abstract_val(v))
#define OutputFormat_val(v) (*(const AVOutputFormat **)Data_abstract_val(v))

/* Format values. Each raises a failure on a null pointer. */
value ocaml_av_wrap_input_format(const AVInputFormat *format);
value ocaml_av_wrap_output_format(const AVOutputFormat *format);

#endif
