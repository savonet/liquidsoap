#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/custom.h>

#include <string.h>
#include <assert.h>

static value copy_buffer(char *buf, int len)
{
  value ans = caml_alloc_string(len);
  memcpy(String_val(ans), buf, len);
  return ans;
}

typedef struct
{
  int width;  /* Width in pixels */
  int height; /* Height in pixels */
  unsigned char *data;
} frame;

#define Red(rgb,i,j)     rgb->data[3*(j*rgb->width + i)]
#define Green(rgb,i,j)   rgb->data[3*(j*rgb->width + i)+1]
#define Blue(rgb,i,j)    rgb->data[3*(j*rgb->width + i)+2]
#define Pixel(rgb,i,j)   {Red(rgb,i,j),Blue(rgb,i,j),Green(rgb,i,j)}

#define Frame_val(v) (*((frame**)Data_custom_val(v)))

static void finalize_frame(value v)
{
  frame *f = Frame_val(v);
  free(f->data);
  free(f);
}

static struct custom_operations frame_ops =
{
  "liquidsoap_rgb_frame",
  finalize_frame,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

CAMLprim value caml_rgb_create(value width, value height)
{
  CAMLparam0();
  CAMLlocal1(ret);
  frame *rgb = malloc(sizeof(frame));

  rgb->width = Int_val(width);
  rgb->height = Int_val(height);
  int len = rgb->width * rgb->height;
  rgb->data = malloc(3 * len * sizeof(unsigned char));

  ret = caml_alloc_custom(&frame_ops, sizeof(frame*), 1, 0);
  Frame_val(ret) = rgb;

  CAMLreturn(ret);
}

CAMLprim value caml_rgb_copy(value _src)
{
  CAMLparam1(_src);
  CAMLlocal1(ans);
  frame *src = Frame_val(_src);
  ans = caml_rgb_create(src->width, src->height);
  frame *dst = Frame_val(ans);

  memcpy(dst->data, src->data, 3 * src->width * src->height);

  CAMLreturn(ans);
}

CAMLprim value caml_rgb_blit(value _src, value _dst)
{
  CAMLparam2(_src, _dst);
  frame *src = Frame_val(_src),
        *dst = Frame_val(_dst);

  assert(dst->width == src->width);
  assert(dst->height == src->height);
  memcpy(dst->data, src->data, 3 * src->width * src->height);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_fill(value f, value col)
{
  CAMLparam2(f, col);
  frame *rgb = Frame_val(f);
  int r = Int_val(Field(col, 0)),
      g = Int_val(Field(col, 1)),
      b = Int_val(Field(col, 2));
  int i,j;

  caml_enter_blocking_section();
  for (j = 0; j < rgb->height; j++)
    for (i = 0; i < rgb->width; i++)
    {
      Red(rgb,i,j)   = r;
      Green(rgb,i,j) = g;
      Blue(rgb,i,j)  = b;
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

// TODO: Implements ASM version of these conversions,
// See:  http://svn.netlabs.org/repos/wvgui/trunk/yuv/

#define CLIP(color) (unsigned char)(((color)>0xFF)?0xff:(((color)<0)?0:(color)))

void YUV420_to_RGB(unsigned char *ysrc, unsigned char *usrc, unsigned char *vsrc, frame *rgb)
{
/* From libv4l code. */
  int i,j;
  unsigned char *data = rgb->data;

  for (i = 0; i < rgb->height; i++) {
    for (j = 0; j < rgb->width; j += 2) {
      /* fast slightly less accurate multiplication free code */
      int u1 = (((*usrc - 128) << 7) +  (*usrc - 128)) >> 6;
      int rg = (((*usrc - 128) << 1) +  (*usrc - 128) +
                ((*vsrc - 128) << 2) + ((*vsrc - 128) << 1)) >> 3;
      int v1 = (((*vsrc - 128) << 1) +  (*vsrc - 128)) >> 1;

      *data++ = CLIP(*ysrc + v1);
      *data++ = CLIP(*ysrc - rg);
      *data++ = CLIP(*ysrc + u1);
      ysrc++;

      *data++ = CLIP(*ysrc + v1);
      *data++ = CLIP(*ysrc - rg);
      *data++ = CLIP(*ysrc + u1);

      ysrc++;
      usrc++;
      vsrc++;
    }
    /* Rewind u and v for next line */
    if (i&1) {
      usrc -= rgb->width / 2;
      vsrc -= rgb->width / 2;
    }
  }
}

// TODO: implement multiplication-free version of 
// this conversion, as well as ASM optimized ones..

/* From Kamelia's source code.
 * http://sourceforge.net/projects/kamaelia
 */
void RGB_to_YUV420(frame *rgb,
                   unsigned char *y_output,
                   unsigned char *u_output,
                   unsigned char *v_output)
{
    int R, G, B;
    int Y, U, V;

    int row;
    int col;

    int *uline;
    int *vline;
    int *ubuf;
    int *vbuf;

    int *ulineptr;
    int *vlineptr;
    int *ubufptr;
    int *vbufptr;

    unsigned char *data = rgb->data;

    int halfwidth;
    halfwidth = rgb->width>>1;

    // allocate temporary buffers for filtering U and V components to allow
    // sensible downsampling
    uline = vline = ubuf = vbuf = NULL;

    uline = (int *)calloc( rgb->width+2, sizeof(int) );
    vline = (int *)calloc( rgb->width+2, sizeof(int) );
    ubuf  = (int *)calloc( halfwidth*(rgb->height+2), sizeof(int) );
    vbuf  = (int *)calloc( halfwidth*(rgb->height+2), sizeof(int) );

    assert (uline && vline && ubuf && vbuf);

    // pre-pad buffers with default 'zero' values (128)
    uline[0] = uline[rgb->width+1] = 128;
    vline[0] = vline[rgb->width+1] = 128;
    for(col=0; col<halfwidth; col++)
    {
        ubuf[col] = ubuf[col + halfwidth*(rgb->height+1)] = 128;
        vbuf[col] = ubuf[col + halfwidth*(rgb->height+1)] = 128;
    }

    // offset base addresses
    uline = uline + 1;
    vline = vline + 1;
    ubuf = ubuf + halfwidth;
    vbuf = vbuf + halfwidth;

    ubufptr = ubuf;
    vbufptr = vbuf;
    for (row=0; row<rgb->height; row=row+1)
    {
        ulineptr = uline;
        vlineptr = vline;

        for(col=0; col<rgb->width; col++)
        {
            // even numbered pixel
            R = (int)(*(data++));
            G = (int)(*(data++));
            B = (int)(*(data++));

            Y = (( 66*R + 129*G +  25*B + 128)>>8)+ 16;
            U = ((-38*R -  74*G + 112*B + 128)>>8)+128;
            V = ((112*R -  94*G -  18*B + 128)>>8)+128;

            *(y_output++) = (unsigned char)( (Y<0) ? 0 : ((Y>255) ? 255 : Y) );
            *(ulineptr++) = U;
            *(vlineptr++) = V;
        }

        for(col=0; col<rgb->width; col=col+2)
        {
            *(ubufptr++) = ( uline[col-1] + 2*uline[col] + uline[col+1] )>>2;
            *(vbufptr++) = ( vline[col-1] + 2*vline[col] + vline[col+1] )>>2;
        }
    }

    ubufptr = ubuf;
    vbufptr = vbuf;
    for (row=0; row<rgb->height; row=row+2)
    {
        for(col=0; col<halfwidth; col++)
        {
            U = ( ubufptr[-halfwidth] + 2*(*ubufptr) + ubufptr[+halfwidth] )>>2;
            V = ( vbufptr[-halfwidth] + 2*(*vbufptr) + vbufptr[+halfwidth] )>>2;

            *(u_output++) = (unsigned char)( (U<0) ? 0 : ((U>255) ? 255 : U) );
            *(v_output++) = (unsigned char)( (V<0) ? 0 : ((V>255) ? 255 : V) );

            ubufptr++;
            vbufptr++;
        }
        ubufptr += halfwidth;
        vbufptr += halfwidth;
    }

    uline = uline - 1;
    vline = vline - 1;
    ubuf = ubuf - halfwidth;
    vbuf = vbuf - halfwidth;

    free(uline);
    free(vline);
    free(ubuf);
    free(vbuf);
}

CAMLprim value caml_rgb_of_YUV420(value yuv, value dst)
{
  CAMLparam2(yuv, dst);
  frame *rgb = Frame_val(dst);
  int ylen = caml_string_length(Field(yuv, 0)),
      ulen = caml_string_length(Field(yuv, 1)),
      vlen = caml_string_length(Field(yuv, 2));
  unsigned char *y = malloc(ylen);
  unsigned char *u = malloc(ulen);
  unsigned char *v = malloc(vlen);

  memcpy(y, String_val(Field(yuv, 0)), ylen);
  memcpy(u, String_val(Field(yuv, 1)), ulen);
  memcpy(v, String_val(Field(yuv, 2)), vlen);

  caml_enter_blocking_section();
  YUV420_to_RGB(y, u, v, rgb);
  caml_leave_blocking_section();

  memcpy(String_val(Field(yuv, 0)), y, ylen);
  memcpy(String_val(Field(yuv, 0)), u, ulen);
  memcpy(String_val(Field(yuv, 0)), v, vlen);
  free(y);
  free(u);
  free(v);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_to_YUV420(value f)
{
  CAMLparam1(f);
  CAMLlocal1(ans);
  frame *rgb = Frame_val(f);
  int len = rgb->width * rgb->height;
  unsigned char *y = malloc(len),
                *u = malloc(len/4),
                *v = malloc(len/4);

  caml_enter_blocking_section();
  RGB_to_YUV420(rgb, y, u, v);
  caml_leave_blocking_section();

  ans = caml_alloc_tuple(3);
  Store_field(ans, 0, copy_buffer((char*)y, len));
  Store_field(ans, 1, copy_buffer((char*)u, len/4));
  Store_field(ans, 2, copy_buffer((char*)v, len/4));
  free(y);
  free(u);
  free(v);

  CAMLreturn(ans);
}

CAMLprim value caml_rgb_get(value f, value _x, value _y)
{
  CAMLparam1(f);
  CAMLlocal1(ans);
  frame *rgb = Frame_val(f);
  int x = Int_val(_x), y = Int_val(_y);
  unsigned char pix[3] = Pixel(rgb,x,y);

  ans = caml_alloc_tuple(3);
  Store_field(ans, 0, Val_int(pix[0]));
  Store_field(ans, 1, Val_int(pix[1]));
  Store_field(ans, 2, Val_int(pix[2]));

  CAMLreturn(ans);
}

CAMLprim value caml_rgb_set(value f, value _x, value _y, value _rgb)
{
  CAMLparam2(f, _rgb);
  CAMLlocal1(ans);
  frame *rgb = Frame_val(f);
  int x = Int_val(_x), y = Int_val(_y);
  int r = Int_val(Field(_rgb, 0));
  int g = Int_val(Field(_rgb, 1));
  int b = Int_val(Field(_rgb, 2));

  Red(rgb,x,y) = r;
  Green(rgb,x,y) = g;
  Blue(rgb,x,y) = b;

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_randomize(value f)
{
  frame *rgb = Frame_val(f);
  int len = rgb->width * rgb->height;
  int i;

  caml_enter_blocking_section();
  for (i = 0; i < 3 * len; i++)
    rgb->data[i] = rand();
  caml_leave_blocking_section();

  return Val_unit;
}

CAMLprim value caml_rgb_scale(value _dst, value _src)
{
  CAMLparam2(_dst, _src);
  frame *dst = Frame_val(_dst), *src = Frame_val(_src);
  int i, j, c;

  caml_enter_blocking_section();
  for (j = 0; j < dst->height; j++)
    for (i = 0; i < dst->width; i++)
      for (c = 0; c < 3; c++)
        dst->data[3 * (j * dst->width + i) + c] =
          src->data[3 * (src->width * (j * src->height / dst->height) + i * src->width / dst->width) + c];
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

static void bmp_pint32(char *dst, int n)
{
  dst[0] = n & 0xff;
  dst[1] = (n >> 8) & 0xff;
  dst[2] = (n >> 16) & 0xff;
  dst[3] = (n >> 24) & 0xff;
}

static void bmp_pint16(char *dst, int n)
{
  dst[0] = n & 0xff;
  dst[1] = (n >> 8) & 0xff;
}

/* See http://en.wikipedia.org/wiki/BMP_file_format */
CAMLprim value caml_rgb_to_bmp(value _rgb)
{
  CAMLparam1(_rgb);
  CAMLlocal1(ans);
  frame *rgb = Frame_val(_rgb);
  int len = rgb->width * rgb->height;
  char *bmp = malloc(54 + 3 * len);
  int i, j, c;

  caml_enter_blocking_section();
  bmp[0]='B';
  bmp[1]='M';
  bmp_pint32(bmp+2 , 54 + 3 * len);
  bmp_pint16(bmp+6 , 0);
  bmp_pint16(bmp+8 , 0);
  bmp_pint32(bmp+10, 54);
  bmp_pint32(bmp+14, 40);
  bmp_pint32(bmp+18, rgb->width);
  bmp_pint32(bmp+22, rgb->height);
  bmp_pint16(bmp+26, 1);
  bmp_pint16(bmp+28, 24);
  bmp_pint32(bmp+30, 0);
  bmp_pint32(bmp+34, 3*len);
  bmp_pint32(bmp+38, 2834);
  bmp_pint32(bmp+42, 2834);
  bmp_pint32(bmp+46, 0);
  bmp_pint32(bmp+50, 0);

  for(j = 0; j < rgb->height; j++)
    for(i = 0; i < rgb->width; i++)
      for(c = 0; c < 3; c++)
        bmp[3 * ((rgb->height - j - 1) * rgb->width + i) + c + 54] =
          rgb->data[3 * (j * rgb->width + i) + (2 - c)];
  caml_leave_blocking_section();

  ans = caml_alloc_string(54 + 3 * len);
  memcpy(String_val(ans), bmp, 54 + 3 * len);
  free(bmp);

  CAMLreturn(ans);
}

CAMLprim value caml_rgb_to_color_array(value _rgb)
{
  CAMLparam1(_rgb);
  CAMLlocal2(ans, line);
  frame *rgb = Frame_val(_rgb);
  int i, j, c;

  ans = caml_alloc_tuple(rgb->height);
  for(j=0; j < rgb->height; j++)
  {
    line = caml_alloc_tuple(rgb->width);
    for(i=0; i < rgb->width; i++)
    {
      c = (Red(rgb,i,j) << 16)
        + (Green(rgb,i,j) << 8)
        + Blue(rgb,i,j);
      Store_field(line, i, Val_int(c));
    }
    Store_field(ans, j, line);
  }

  CAMLreturn(ans);
}

CAMLprim value caml_rgb_greyscale(value _rgb)
{
  CAMLparam1(_rgb);
  frame *rgb = Frame_val(_rgb);
  int i,j;
  unsigned char c;

  caml_enter_blocking_section();
  for (j = 0; j < rgb->height; j++)
    for (i = 0; i < rgb->width; i++)
    {
      c = (Red(rgb,i,j)
        +  Green(rgb,i,j)
        +  Blue(rgb,i,j)) / 3;
      Red(rgb,i,j)   = c;
      Green(rgb,i,j) = c;
      Blue(rgb,i,j)  = c;
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}
