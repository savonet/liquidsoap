#include <libswresample/swresample.h>

#include "avutil_stubs.h"

/*
 * Enum representing different kinds of vectors used in swresample operations.
 *
 * Members:
 *   Str   - Vector of strings
 *   P_Str - Vector of pointers to strings
 *   Fa    - Vector of float arrays (audio samples)
 *   P_Fa  - Vector of pointers to float arrays
 *   Ba    - Vector of byte arrays
 *   P_Ba  - Vector of pointers to byte arrays
 *   Frm   - Vector of frames
 */
typedef enum _vector_kind { Str, P_Str, Fa, P_Fa, Ba, P_Ba, Frm } vector_kind;

typedef struct swr_t swr_t;
