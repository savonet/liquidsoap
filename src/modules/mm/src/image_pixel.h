#define CLIP(color)                                                            \
  (unsigned char)(((color) > 0xff) ? 0xff : (((color) < 0) ? 0 : (color)))

// studio swing
/* #define YofRGB(r,g,b) CLIP(((66 * r + 129 * g +  25 * b + 128) >> 8) +  16)
 */
/* #define UofRGB(r,g,b) CLIP(((-38 * r -  74 * g + 112 * b + 128) >> 8) + 128)
 */
/* #define UofRGB(r,g,b) CLIP(((112 * r -  94 * g -  18 * b + 128) >> 8) + 128)
 */
/* #define RofYUV(y,u,v) CLIP((298 * (y - 16) + 409 * (v - 128) + 128) >> 8) */
/* #define GofYUV(y,u,v) CLIP((298 * (y - 16) - 100 * (u - 128) - 208 * (v -
 * 128) + 128) >> 8) */
/* #define BofYUV(y,u,v) CLIP((298 * (y - 16) + 516 * (u - 128) + 128) >> 8) */

// full swing
#define YofRGB(r, g, b) CLIP((19595 * r + 38470 * g + 7471 * b) >> 16)
#define UofRGB(r, g, b)                                                        \
  CLIP((36962 * (b - CLIP((19595 * r + 38470 * g + 7471 * b) >> 16)) >> 16) +  \
       128)
#define VofRGB(r, g, b)                                                        \
  CLIP((46727 * (r - CLIP((19595 * r + 38470 * g + 7471 * b) >> 16)) >> 16) +  \
       128)
#define RofYUV(y, u, v) CLIP(y + (91881 * v >> 16) - 179)
#define GofYUV(y, u, v) CLIP(y - ((22544 * u + 46793 * v) >> 16) + 135)
#define BofYUV(y, u, v) CLIP(y + (116129 * u >> 16) - 226)
