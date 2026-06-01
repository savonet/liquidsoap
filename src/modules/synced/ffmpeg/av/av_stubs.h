#ifndef _AV_STUBS_H_
#define _AV_STUBS_H_

#include <caml/mlvalues.h>

#include <libavformat/avformat.h>

AVFormatContext *ocaml_av_get_format_context(value *p_av);

#if LIBAVFORMAT_VERSION_INT <= AV_VERSION_INT(59, 0, 100)
#define avioformat_const
#else
#define avioformat_const const
#endif

/***** AVInputFormat *****/

#define InputFormat_val(v)                                                     \
  (*(avioformat_const AVInputFormat **)Data_abstract_val(v))

void value_of_inputFormat(value *p_value,
                          avioformat_const AVInputFormat *inputFormat);

/***** AVOutputFormat *****/

#define OutputFormat_val(v)                                                    \
  (*(avioformat_const AVOutputFormat **)Data_abstract_val(v))

void value_of_outputFormat(value *p_value,
                           avioformat_const AVOutputFormat *outputFormat);

/***** Control message *****/
value *ocaml_av_get_control_message_callback(struct AVFormatContext *ctx);

void ocaml_av_set_control_message_callback(value *p_av,
                                           av_format_control_message c_callback,
                                           value *p_ocaml_callback);

#endif // _AV_STUBS_H_
