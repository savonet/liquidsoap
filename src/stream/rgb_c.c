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
#include <math.h>

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

#define Rgb_num_pix(rgb)    rgb->width * rgb->height
#define Rgb_colors          3
#define Rgb_elems_per_pixel 4
#define Rgb_num_elem(rgb)   Rgb_elems_per_pixel * Rgb_num_pix(rgb)
#define Rgb_data_size(rgb)  Rgb_num_elem(rgb) * sizeof(unsigned char)
#define Color(rgb,c,i,j)    rgb->data[Rgb_elems_per_pixel * (j * rgb->width + i) + c]
#define Red(rgb,i,j)        Color(rgb,0,i,j)
#define Green(rgb,i,j)      Color(rgb,1,i,j)
#define Blue(rgb,i,j)       Color(rgb,2,i,j)
#define Alpha(rgb,i,j)      Color(rgb,3,i,j)
#define Pixel(rgb,i,j)      {Red(rgb,i,j),Blue(rgb,i,j),Green(rgb,i,j),Alpha(rgb,i,j)}
#define Space_clip_color(rgb,c,i,j) (i<0||j<0||i>=rgb->width||j>=rgb->height)?0:Color(rgb,c,i,j)

#define assert_same_dim(src, dst) { assert(dst->width == src->width); assert(dst->height == src->height); }

#define Frame_val(v) (*((frame**)Data_custom_val(v)))

static void rgb_free(frame *f)
{
  free(f->data);
  free(f);
}

static void finalize_frame(value v)
{
  frame *f = Frame_val(v);
  rgb_free(f);
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
  rgb->data = malloc(Rgb_data_size(rgb));

  ret = caml_alloc_custom(&frame_ops, sizeof(frame*), 1, 0);
  Frame_val(ret) = rgb;

  CAMLreturn(ret);
}

static frame *rgb_copy(frame *src)
{
  frame *dst = malloc(sizeof(frame));
  dst->width = src->width;
  dst->height = src->height;
  dst->data = malloc(Rgb_data_size(src));
  memcpy(dst->data, src->data, Rgb_data_size(src));

  return dst;
}

CAMLprim value caml_rgb_copy(value _src)
{
  CAMLparam1(_src);
  CAMLlocal1(ans);
  frame *src = Frame_val(_src);
  ans = caml_rgb_create(src->width, src->height);
  frame *dst = Frame_val(ans);

  memcpy(dst->data, src->data, Rgb_data_size(src));

  CAMLreturn(ans);
}

CAMLprim value caml_rgb_get_width(value rgb)
{
  return Val_int(Frame_val(rgb)->width);
}

CAMLprim value caml_rgb_get_height(value rgb)
{
  return Val_int(Frame_val(rgb)->height);
}

CAMLprim value caml_rgb_blit(value _src, value _dst)
{
  CAMLparam2(_src, _dst);
  frame *src = Frame_val(_src),
        *dst = Frame_val(_dst);

  assert_same_dim(src, dst);
  memcpy(dst->data, src->data, Rgb_data_size(src));

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_blit_off(value _src, value _dst, value _dx, value _dy)
{
  CAMLparam2(_src, _dst);
  frame *src = Frame_val(_src),
        *dst = Frame_val(_dst);
  int dx = Int_val(_dx),
      dy = Int_val(_dy);
  int i, j, c;

  caml_enter_blocking_section();
  for (j = 0; j < dst->height; j++)
    for (i = 0; i < dst->width; i++)
      for (c = 0; c < Rgb_elems_per_pixel; c++)
        Color(dst, c, i, j) = Space_clip_color(src, c, (i-dx), (j-dy));
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_fill(value f, value col)
{
  CAMLparam2(f, col);
  frame *rgb = Frame_val(f);
  int r = Int_val(Field(col, 0)),
      g = Int_val(Field(col, 1)),
      b = Int_val(Field(col, 2)),
      a = Int_val(Field(col, 3));
  int i,j;

  caml_enter_blocking_section();
  for (j = 0; j < rgb->height; j++)
    for (i = 0; i < rgb->width; i++)
    {
      Red(rgb,i,j)   = r;
      Green(rgb,i,j) = g;
      Blue(rgb,i,j)  = b;
      Alpha(rgb,i,j) = a;
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

// TODO: Implements ASM version of these conversions,
// See:  http://svn.netlabs.org/repos/wvgui/trunk/yuv/

#define CLIP(color) (unsigned char)(((color)>0xff)?0xff:(((color)<0)?0:(color)))

void YUV420_to_RGB(unsigned char *ysrc, unsigned char *usrc, unsigned char *vsrc, frame *rgb)
{
/* From libv4l code. */
  int i,j;

  for (i = 0; i < rgb->height; i++) {
    for (j = 0; j < rgb->width; j += 2) {
      /* fast slightly less accurate multiplication free code */
      int u1 = (((*usrc - 128) << 7) +  (*usrc - 128)) >> 6;
      int rg = (((*usrc - 128) << 1) +  (*usrc - 128) +
                ((*vsrc - 128) << 2) + ((*vsrc - 128) << 1)) >> 3;
      int v1 = (((*vsrc - 128) << 1) +  (*vsrc - 128)) >> 1;

      Red(rgb,j,i)   = CLIP(*ysrc + v1);
      Green(rgb,j,i) = CLIP(*ysrc - rg);
      Blue(rgb,j,i)  = CLIP(*ysrc + u1);
      Alpha(rgb,j,i) = 0xff;
      ysrc++;

      Red(rgb,j+1,i) = CLIP(*ysrc + v1);
      Green(rgb,j+1,i) = CLIP(*ysrc - rg);
      Blue(rgb,j+1,i) = CLIP(*ysrc + u1);
      Alpha(rgb,j+1,i) = 0xff;

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
    int R, G, B, A;
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
    for (row=0; row<rgb->height; row++)
    {
        ulineptr = uline;
        vlineptr = vline;

        for(col=0; col<rgb->width; col++)
        {
            // even numbered pixel
            R = (int)Red(rgb,col,row);
            G = (int)Green(rgb,col,row);
            B = (int)Blue(rgb,col,row);
            A = (int)Alpha(rgb,col,row);
            R = R * A / 0xff;
            G = G * A / 0xff;
            B = B * A / 0xff;

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
  unsigned char pix[Rgb_elems_per_pixel] = Pixel(rgb,x,y);
  int i;

  ans = caml_alloc_tuple(Rgb_elems_per_pixel);
  for (i = 0; i < Rgb_elems_per_pixel; i++)
    Store_field(ans, i, Val_int(pix[i]));

  CAMLreturn(ans);
}

CAMLprim value caml_rgb_set(value f, value _x, value _y, value _rgb)
{
  CAMLparam2(f, _rgb);
  CAMLlocal1(ans);
  frame *rgb = Frame_val(f);
  int x = Int_val(_x),
      y = Int_val(_y);
  int r = Int_val(Field(_rgb, 0));
  int g = Int_val(Field(_rgb, 1));
  int b = Int_val(Field(_rgb, 2));
  int a = Int_val(Field(_rgb, 3));

  Red(rgb,x,y) = r;
  Green(rgb,x,y) = g;
  Blue(rgb,x,y) = b;
  Alpha(rgb,x,y) = a;

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_randomize(value f)
{
  frame *rgb = Frame_val(f);
  int i, j, c;

  caml_enter_blocking_section();
  for (j = 0; j < rgb->height; j++)
    for (i = 0; i < rgb->width; i++)
      for (c = 0; c < Rgb_colors; c++)
        Color(rgb,c,i,j) = rand();
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
      for (c = 0; c < Rgb_elems_per_pixel; c++)
        Color(dst, c, i, j) = Color(src, c, i * src->width / dst->width, j * src->height / dst->height);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_proportional_scale(value _dst, value _src)
{
  CAMLparam2(_dst, _src);
  frame *dst = Frame_val(_dst), *src = Frame_val(_src);
  int i, j, c;
  int cn, cd, ox, oy;

  if (dst->height * src->width < src->height * dst->width)
  {
    cn = dst->height;
    cd = src->height;
    ox = (dst->width - src->width * cn / cd) / 2;
    oy = 0;
  }
  else
  {
    cn = dst->width;
    cd = src->width;
    ox = 0;
    oy = (dst->height - src->height * cn / cd) / 2;
  }

  caml_enter_blocking_section();
  /* Fill borders in black */
  if (oy == 0)
    for (j = 0; j < dst->height; j++)
      for (c = 0; c < Rgb_elems_per_pixel; c++)
      {
        for (i = 0; i < ox; i++)
          Color(dst, c, i, j) = 0;
        for (i = dst->width - ox; i < dst->width; i++)
          Color(dst, c, i, j) = 0;
      }
  else
    for (i = 0; i < dst->width; i++)
      for (c = 0; c < Rgb_elems_per_pixel; c++)
      {
        for (j = 0; j < oy; j++)
          Color(dst, c, i, j) = 0;
        for (j = dst->height - oy; j < dst->height; j++)
          Color(dst, c, i, j) = 0;
      }
  /* Scale the image */
  for (j = oy; j < dst->height - oy; j++)
    for (i = ox; i < dst->width - ox; i++)
      for (c = 0; c < Rgb_elems_per_pixel; c++)
        Color(dst, c, i, j) = Color(src, c, (i - ox) * cd / cn, (j - oy) * cd / cn);
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
  int len = Rgb_num_pix(rgb);
  char *bmp = malloc(54 + 3 * len);
  int i, j;
  unsigned char a;

  caml_enter_blocking_section();
  bmp[0]='B';                       /* Magic number */
  bmp[1]='M';
  bmp_pint32(bmp+2 , 54 + 3 * len); /* File size */
  bmp_pint16(bmp+6 , 0);            /* Reserved */
  bmp_pint16(bmp+8 , 0);            /* Reserved */
  bmp_pint32(bmp+10, 54);           /* Data offset */
  bmp_pint32(bmp+14, 40);           /* Second header size */
  bmp_pint32(bmp+18, rgb->width);   /* Width */
  bmp_pint32(bmp+22, rgb->height);  /* Height */
  bmp_pint16(bmp+26, 1);            /* Nb of color planes */
  bmp_pint16(bmp+28, 24);           /* BPP */
  bmp_pint32(bmp+30, 0);            /* Compression */
  bmp_pint32(bmp+34, 3 * len);      /* Image size */
  bmp_pint32(bmp+38, 2834);         /* Horizontal resolution */
  bmp_pint32(bmp+42, 2834);         /* Vertical resolution */
  bmp_pint32(bmp+46, 0);            /* Number of colors */
  bmp_pint32(bmp+50, 0);            /* Number of important colors */

  for(j = 0; j < rgb->height; j++)
    for(i = 0; i < rgb->width; i++)
    {
      a = Alpha(rgb, i, j);
      bmp[3 * ((rgb->height - j - 1) * rgb->width + i) + 0 + 54] = Blue(rgb, i, j) * a / 0xff;
      bmp[3 * ((rgb->height - j - 1) * rgb->width + i) + 1 + 54] = Green(rgb, i, j) * a / 0xff;
      bmp[3 * ((rgb->height - j - 1) * rgb->width + i) + 2 + 54] = Red(rgb, i, j) * a / 0xff;
    }
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
  unsigned char a;

  ans = caml_alloc_tuple(rgb->height);
  for(j=0; j < rgb->height; j++)
  {
    line = caml_alloc_tuple(rgb->width);
    for(i=0; i < rgb->width; i++)
    {
      a = Alpha(rgb, i, j);
      c = ((Red(rgb,i,j) * a / 0xff) << 16)
        + ((Green(rgb,i,j) * a / 0xff) << 8)
        + (Blue(rgb,i,j) * a / 0xff);
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

CAMLprim value caml_rgb_add(value _dst, value _src)
{
  CAMLparam2(_dst, _src);
  frame *dst = Frame_val(_dst),
        *src = Frame_val(_src);
  int i, j, c;

  assert_same_dim(src, dst);
  caml_enter_blocking_section();
  for (j = 0; j < dst->height; j++)
    for (i = 0; i < dst->width; i++)
    {
      for (c = 0; c < Rgb_colors; c++)
        Color(dst, c, i, j) = CLIP(Color(src, c, i, j) * Alpha(src,i,j) / 0xff + Color(dst, c, i, j) * (0xff - Alpha(src,i,j)) / 0xff);
      Alpha(dst, i, j) = CLIP(Alpha(src, i, j) + (0xff - Alpha(src, i, j)) * Alpha(dst, i, j));
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_invert(value _rgb)
{
  CAMLparam1(_rgb);
  frame *rgb = Frame_val(_rgb);
  int i, j, c;

  caml_enter_blocking_section();
  for (j = 0; j < rgb->height; j++)
    for (i = 0; i < rgb->width; i++)
      for (c = 0; c < Rgb_colors; c++)
        Color(rgb, c, i, j) = 0xff - Color(rgb, c, i, j);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_scale_opacity(value _rgb, value _x)
{
  CAMLparam1(_rgb);
  frame *rgb = Frame_val(_rgb);
  double x = Double_val(_x);
  int i, j;

  caml_enter_blocking_section();
  for (j = 0; j < rgb->height; j++)
    for (i = 0; i < rgb->width; i++)
      Alpha(rgb, i, j) = CLIP(Alpha(rgb, i, j) * x);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_rotate(value _rgb, value _angle)
{
  CAMLparam1(_rgb);
  frame *rgb = Frame_val(_rgb);
  frame *old = rgb_copy(rgb);
  double a = Double_val(_angle);
  int ox = rgb->width / 2,
      oy = rgb->height / 2;
  int i, j, c,
      i2, j2;

  caml_enter_blocking_section();
  for (j = 0; j < rgb->height; j++)
    for (i = 0; i < rgb->width; i++)
      for (c = 0; c < Rgb_elems_per_pixel; c++)
      {
        i2 = (i - ox) * cos(a) + (j - oy) * sin(a) + ox;
        j2 = -(i - ox) * sin(a) + (j - oy) * cos(a) + oy;
        Color(rgb, c, i, j) = Space_clip_color(old, c, i2, j2);
      }
  rgb_free(old);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_affine(value _rgb, value _ax, value _ay, value _ox, value _oy)
{
  CAMLparam5(_rgb, _ax, _ay, _ox, _oy);
  frame *rgb = Frame_val(_rgb);
  frame *old = rgb_copy(rgb);
  double ax = Double_val(_ax),
         ay = Double_val(_ay);
  int i, j, i2, j2, c;
  int ox = Int_val(_ox),
      oy = Int_val(_oy);
  int dx = rgb->width / 2,  /* Center of scaling */
      dy = rgb->height / 2;

  caml_enter_blocking_section();
  for (j = 0; j < rgb->height; j++)
    for (i = 0; i < rgb->width; i++)
      for (c = 0; c < Rgb_elems_per_pixel; c++)
      {
        i2 = (i - ox - dx) / ax + dx;
        j2 = (j - oy - dy) / ay + dy;
        Color(rgb, c, i, j) = Space_clip_color(old, c, i2, j2);
      }
  rgb_free(old);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_mask(value _rgb, value _mask)
{
  CAMLparam2(_rgb, _mask);
  frame *rgb = Frame_val(_rgb),
        *mask = Frame_val(_mask);
  int i, j;

  assert_same_dim(rgb, mask);
  caml_enter_blocking_section();
  for (j = 0; j < rgb->height; j++)
    for (i = 0; i < rgb->width; i++)
      Alpha(rgb, i, j) =
        CLIP(sqrt(
              Red(mask, i, j) * Red(mask, i, j) +
              Green(mask, i, j) * Green(mask, i, j) +
              Blue(mask, i, j) * Blue(mask, i, j))) *
        Alpha(mask, i, j) / 0xff;
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}
