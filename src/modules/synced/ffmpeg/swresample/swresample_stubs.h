#include <libswresample/swresample.h>

#include "avutil_stubs.h"

typedef enum _vector_kind { Str, P_Str, Fa, P_Fa, Ba, P_Ba, Frm } vector_kind;

typedef struct swr_t swr_t;
