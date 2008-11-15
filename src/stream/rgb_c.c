#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/custom.h>

#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <math.h>

// MMX invert is broken at the moment..
#undef HAVE_MMX

#ifdef HAVE_MMX
#include <mmintrin.h>
#endif

#define max(a,b) (a>b)?a:b
#define min(a,b) (a<b)?a:b

typedef struct
{
  int width;  /* Width in pixels */
  int height; /* Height in pixels */
  int stride; /* Bytes per line */
  unsigned char *data;
} frame;

#define Rgb_num_pix(rgb)    (rgb)->width * (rgb)->height
#define Rgb_colors          3
#define Rgb_elems_per_pixel 4
#define Rgb_num_elem(rgb)   Rgb_elems_per_pixel * Rgb_num_pix(rgb)
#define Rgb_plane_size(rgb) (rgb)->stride * (rgb)->height
#define Rgb_data_size(rgb)  Rgb_plane_size(rgb) * sizeof(unsigned char)
#define Color(rgb,c,i,j)    (rgb)->data[j * (rgb)->stride + Rgb_elems_per_pixel * (i) + c]
#define Red(rgb,i,j)        Color(rgb,0,i,j)
#define Green(rgb,i,j)      Color(rgb,1,i,j)
#define Blue(rgb,i,j)       Color(rgb,2,i,j)
#define Alpha(rgb,i,j)      Color(rgb,3,i,j)
#define Pixel(rgb,i,j)      {Red(rgb,i,j),Blue(rgb,i,j),Green(rgb,i,j),Alpha(rgb,i,j)}
#define Is_outside(rgb,i,j) (i<0||j<0||i>=(rgb)->width||j>=(rgb)->height)
#define Space_clip_color(rgb,c,i,j) (Is_outside(rgb,i,j))?0:Color(rgb,c,i,j)

#define assert_same_dim(src, dst) { assert((dst)->width == (src)->width); assert((dst)->height == (src)->height); }

static frame *frame_of_value(value v, frame *f)
{
  value ba = Field(v,0);
  f->data = Caml_ba_data_val(ba);
  f->width = Int_val(Field(v,1));
  f->height = Int_val(Field(v,2));
  f->stride = Int_val(Field(v,3));

  return f;
}

static void rgb_free(frame *f)
 {
  free(f->data);
 }

static inline void rgb_blank(frame *rgb)
{
  memset(rgb->data, 0, Rgb_data_size(rgb));
}

static frame *rgb_copy(frame *src, frame *dst)
{
  dst->width = src->width;
  dst->height = src->height;
  dst->stride = src->stride;
  dst->data = malloc(Rgb_data_size(src));
  memcpy(dst->data, src->data, Rgb_data_size(src));

  return dst;
}

CAMLprim value caml_rgb_blank(value _rgb)
{
  frame rgb;

  rgb_blank(frame_of_value(_rgb,&rgb));

  return Val_unit;
}

CAMLprim value caml_rgb_blit(value _src, value _dst)
{
  frame src,dst;
  frame_of_value(_src, &src);
  frame_of_value(_dst, &dst);

  assert_same_dim(&src, &dst);
  memcpy(dst.data, src.data, Rgb_data_size(&src));

  return Val_unit;
}

CAMLprim value caml_rgb_blit_off(value _src, value _dst, value _dx, value _dy, value _blank)
{
  frame src,dst;
  frame_of_value(_src, &src);
  frame_of_value(_dst, &dst);

  int dx = Int_val(_dx),
      dy = Int_val(_dy);
  int blank = Bool_val(_blank);
  int i, j, c;
  int istart = max(0, dx),
      iend = min(dst.width, src.width + dx),
      jstart = max(0, dy),
      jend = min(dst.height, src.height + dy);

  caml_register_global_root(&_dst);
  caml_register_global_root(&_src);
  caml_enter_blocking_section();
  /* Blank what's outside src */
  if (blank)
  /*
    for (j = 0; j < dst.height; j++)
    {
      for (i = 0; i < dst.width; i++)
      {
        if (j < jend && j > jstart && i == istart)
        {
          if (iend == dst.width)
            break;
          else
            i = iend;
        }
        for (c = 0; c < Rgb_elems_per_pixel; c++)
          Color(&dst, c, i, j) = 0;
      }
    }
  */
    /* This one seems to be much faster... */
    rgb_blank(&dst);
  /* Copy src to dst for the rest */
  for (j = jstart; j < jend; j++)
    for (i = istart; i < iend; i++)
      for (c = 0; c < Rgb_elems_per_pixel; c++)
        Color(&dst, c, i, j) = Color(&src, c, (i-dx), (j-dy));
  caml_leave_blocking_section();
  caml_remove_global_root(&_dst);
  caml_remove_global_root(&_src);

  return Val_unit;
}

CAMLprim value caml_rgb_blit_off_scale(value _src, value _dst, value d, value dim, value _blank)
{
  frame src,dst;
  frame_of_value(_src, &src);
  frame_of_value(_dst, &dst);

  int dx = Int_val(Field(d, 0)),
      dy = Int_val(Field(d, 1)),
      w = Int_val(Field(dim, 0)),
      h = Int_val(Field(dim, 1));
  int blank = Bool_val(_blank);
  int i, j, c;
  int istart = max(0, dx),
      iend = min(dst.width, w + dx),
      jstart = max(0, dy),
      jend = min(dst.height, h + dy);

  caml_register_global_root(&_dst);
  caml_register_global_root(&_src);
  caml_enter_blocking_section();
  if (blank)
    rgb_blank(&dst);
  for (j = jstart; j < jend; j++)
    for (i = istart; i < iend; i++)
      for (c = 0; c < Rgb_elems_per_pixel; c++)
        Color(&dst, c, i, j) = Color(&src, c, (i-dx)*src.width/w, (j-dy)*src.height/h);
  caml_leave_blocking_section();
  caml_remove_global_root(&_dst);
  caml_remove_global_root(&_src);

  return Val_unit;
}


CAMLprim value caml_rgb_fill(value f, value col)
{
  frame rgb;
  frame_of_value(f,&rgb);
  int r = Int_val(Field(col, 0)),
      g = Int_val(Field(col, 1)),
      b = Int_val(Field(col, 2)),
      a = Int_val(Field(col, 3));
  int i,j;

  caml_register_global_root(&f);
  caml_enter_blocking_section();
  for (j = 0; j < rgb.height; j++)
    for (i = 0; i < rgb.width; i++)
    {
      Red(&rgb,i,j)   = r;
      Green(&rgb,i,j) = g;
      Blue(&rgb,i,j)  = b;
      Alpha(&rgb,i,j) = a;
    }
  caml_leave_blocking_section();
  caml_remove_global_root(&f);

  return Val_unit;
}

// TODO: Implements ASM version of these conversions,
// See:  http://svn.netlabs.org/repos/wvgui/trunk/yuv/

#define CLIP(color)  (unsigned char)(((color)>0xff)?0xff:(((color)<0)?0:(color)))
#define POS(x,o)     ((x << 1)+o)
#define PIX(p,s,x,y) p[y*s+x]

void YUV420_to_RGB(unsigned char *ysrc, int y_stride, unsigned char *usrc, 
                   unsigned char *vsrc, int uv_stride, frame *rgb)
{
/* From libv4l code. */
  int i,j;

  for (i = 0; i < rgb->height / 2; i++) {
    for (j = 0; j < rgb->width / 2; j++) {
      /* fast slightly less accurate multiplication free code */
      int u1 = (((PIX(usrc,uv_stride,j,i) - 128) << 7) +  (PIX(usrc,uv_stride,j,i) - 128)) >> 6;
      int rg = (((PIX(usrc,uv_stride,j,i) - 128) << 1) +  (PIX(usrc,uv_stride,j,i) - 128) +
                ((PIX(vsrc,uv_stride,j,i) - 128) << 2) + ((PIX(vsrc,uv_stride,j,i) - 128) << 1)) >> 3;
      int v1 = (((PIX(vsrc,uv_stride,j,i) - 128) << 1) +  (PIX(vsrc,uv_stride,j,i) - 128)) >> 1;

      Red(rgb,POS(j,0),POS(i,0))   = CLIP(PIX(ysrc,y_stride,POS(j,0),POS(i,0)) + v1);
      Green(rgb,POS(j,0),POS(i,0)) = CLIP(PIX(ysrc,y_stride,POS(j,0),POS(i,0)) - rg);
      Blue(rgb,POS(j,0),POS(i,0))  = CLIP(PIX(ysrc,y_stride,POS(j,0),POS(i,0)) + u1);
      Alpha(rgb,POS(j,0),POS(i,0)) = 0xff;

      Red(rgb,POS(j,1),POS(i,0))   = CLIP(PIX(ysrc,y_stride,POS(j,1),POS(i,0)) + v1);
      Green(rgb,POS(j,1),POS(i,0)) = CLIP(PIX(ysrc,y_stride,POS(j,1),POS(i,0)) - rg);
      Blue(rgb,POS(j,1),POS(i,0))  = CLIP(PIX(ysrc,y_stride,POS(j,1),POS(i,0)) + u1);
      Alpha(rgb,POS(j,1),POS(i,0)) = 0xff;

      Red(rgb,POS(j,0),POS(i,1))   = CLIP(PIX(ysrc,y_stride,POS(j,0),POS(i,1)) + v1);
      Green(rgb,POS(j,0),POS(i,1)) = CLIP(PIX(ysrc,y_stride,POS(j,0),POS(i,1)) - rg);
      Blue(rgb,POS(j,0),POS(i,1))  = CLIP(PIX(ysrc,y_stride,POS(j,0),POS(i,1)) + u1);
      Alpha(rgb,POS(j,0),POS(i,1)) = 0xff;

      Red(rgb,POS(j,1),POS(i,1))   = CLIP(PIX(ysrc,y_stride,POS(j,1),POS(i,1)) + v1);
      Green(rgb,POS(j,1),POS(i,1)) = CLIP(PIX(ysrc,y_stride,POS(j,1),POS(i,1)) - rg);
      Blue(rgb,POS(j,1),POS(i,1))  = CLIP(PIX(ysrc,y_stride,POS(j,1),POS(i,1)) + u1);
      Alpha(rgb,POS(j,1),POS(i,1)) = 0xff;

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
  frame rgb;
  frame_of_value(dst, &rgb);
  value y_val = Field(yuv, 0);
  unsigned char *y = Caml_ba_data_val(Field(y_val, 0));
  int y_stride = Int_val(Field(y_val, 1));
  value uv_val = Field(yuv, 1);
  unsigned char *u = Caml_ba_data_val(Field(uv_val, 0));
  unsigned char *v = Caml_ba_data_val(Field(uv_val, 1));
  int uv_stride = Int_val(Field(uv_val, 2));

  caml_register_global_root(&yuv);
  caml_register_global_root(&dst);

  /* TODO: check the size of the data */
  caml_enter_blocking_section();
  YUV420_to_RGB(y, y_stride, u, v, uv_stride, &rgb);
  caml_leave_blocking_section();

  caml_remove_global_root(&yuv);
  caml_remove_global_root(&dst);

  return Val_unit;
}

CAMLprim value caml_yuv_create(value w, value h)
{
  CAMLparam0();
  CAMLlocal2(tmp,ans);
  int width = Int_val(w);
  int height = Int_val(h);
  intnat len = width * height;
  unsigned char *y = malloc(len),
                *u = malloc(len/4),
                *v = malloc(len/4);

  ans = caml_alloc_tuple(2);
  tmp = caml_alloc_tuple(2);
  Store_field(tmp, 0, caml_ba_alloc(CAML_BA_C_LAYOUT | CAML_BA_UINT8 | CAML_BA_MANAGED, 1, y, &len));
  Store_field(tmp, 1, Val_int(width));
  Store_field(ans, 0, tmp);
  len /= 4;
  tmp = caml_alloc_tuple(3);
  Store_field(tmp, 0, caml_ba_alloc(CAML_BA_C_LAYOUT | CAML_BA_UINT8 | CAML_BA_MANAGED, 1, u, &len));
  Store_field(tmp, 1, caml_ba_alloc(CAML_BA_C_LAYOUT | CAML_BA_UINT8 | CAML_BA_MANAGED, 1, v, &len));
  Store_field(tmp, 2, Val_int(width / 2));
  Store_field(ans, 1, tmp);

  CAMLreturn(ans);
}

CAMLprim value caml_yuv_blank(value f)
{
  CAMLparam1(f);
  CAMLlocal1(tmp);
  struct caml_ba_array *ba;
 
  /* Y data */
  tmp = Field(f,0);
  ba = Caml_ba_array_val(Field(tmp,0));
  memset(ba->data,0,ba->dim[0]);

  /* UV data */
  tmp = Field(f,1);
  ba = Caml_ba_array_val(Field(tmp,0));
  memset(ba->data,0,ba->dim[0]);
  ba = Caml_ba_array_val(Field(tmp,1));
  memset(ba->data,0,ba->dim[0]);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_to_YUV420(value f, value yuv)
{
  frame rgb;
  frame_of_value(f, &rgb);
  value tmp = Field(yuv, 0);
  unsigned char *y = Caml_ba_data_val(Field(tmp,0));
  tmp = Field(yuv,1);
  unsigned char *u = Caml_ba_data_val(Field(tmp,0));
  unsigned char *v = Caml_ba_data_val(Field(tmp,1));

  caml_register_global_root(&yuv);
  caml_register_global_root(&f);

  caml_enter_blocking_section();
  RGB_to_YUV420(&rgb, y, u, v);
  caml_leave_blocking_section();

  caml_remove_global_root(&yuv);
  caml_remove_global_root(&f);

  return Val_unit;
}

CAMLprim value caml_rgb_of_linear_rgb(value _rgb, value _data)
{
  frame rgb;
  frame_of_value(_rgb, &rgb);
  char *data = String_val(_data);
  int i, j;

  /* TODO: blocking section */
  for (j = 0; j < rgb.height; j++)
    for (i = 0; i < rgb.width; i++)
    {
      Red(&rgb,i,j) = data[3 * j * rgb.width + i + 0];
      Green(&rgb,i,j) = data[3 * j * rgb.width + i + 1];
      Blue(&rgb,i,j) = data[3 * j * rgb.width + i + 2];
      Alpha(&rgb,i,j) = 0xff;
    }

  return Val_unit;
}

CAMLprim value caml_rgb_get_pixel(value f, value _x, value _y)
{
  CAMLparam1(f);
  CAMLlocal1(ans);
  frame rgb;
  frame_of_value(f, &rgb);
  int x = Int_val(_x), y = Int_val(_y);
  unsigned char pix[Rgb_elems_per_pixel] = Pixel(&rgb,x,y);
  int i;

  ans = caml_alloc_tuple(Rgb_elems_per_pixel);
  for (i = 0; i < Rgb_elems_per_pixel; i++)
    Store_field(ans, i, Val_int(pix[i]));

  CAMLreturn(ans);
}

CAMLprim value caml_rgb_set_pixel(value f, value _x, value _y, value _rgb)
{
  frame rgb;
  frame_of_value(f, &rgb);
  int x = Int_val(_x),
      y = Int_val(_y);
  int r = Int_val(Field(_rgb, 0));
  int g = Int_val(Field(_rgb, 1));
  int b = Int_val(Field(_rgb, 2));
  int a = Int_val(Field(_rgb, 3));

  Red(&rgb,x,y) = r;
  Green(&rgb,x,y) = g;
  Blue(&rgb,x,y) = b;
  Alpha(&rgb,x,y) = a;

  return Val_unit;
}

CAMLprim value caml_rgb_randomize(value f)
{
  frame rgb;
  frame_of_value(f, &rgb);
  int i, j, c;

  caml_register_global_root(&f);
  caml_enter_blocking_section();
  for (j = 0; j < rgb.height; j++)
    for (i = 0; i < rgb.width; i++)
      for (c = 0; c < Rgb_colors; c++)
        Color(&rgb,c,i,j) = rand();
  caml_leave_blocking_section();
  caml_remove_global_root(&f);

  return Val_unit;
}

CAMLprim value caml_rgb_scale(value _dst, value _src, value xscale, value yscale)
{
  frame src,dst;
  frame_of_value(_src, &src);
  frame_of_value(_dst, &dst);
  int i, j, c;
  int xn = Int_val(Field(xscale, 0)),
      xd = Int_val(Field(xscale, 1)),
      yn = Int_val(Field(yscale, 0)),
      yd = Int_val(Field(yscale, 1));
  int ox = (dst.width - src.width * xn / xd) / 2,
      oy = (dst.height - src.height * yn / yd) / 2;

  assert(ox >= 0 && oy >= 0);

  caml_register_global_root(&_dst);
  caml_register_global_root(&_src);
  caml_enter_blocking_section();
  if (ox != 0 || oy != 0)
    rgb_blank(&dst);
  for (j = oy; j < dst.height - oy; j++)
    for (i = ox; i < dst.width - ox; i++)
      for (c = 0; c < Rgb_elems_per_pixel; c++)
        Color(&dst, c, i, j) = Color(&src, c, (i - ox) * xd / xn, (j - oy) * yd / yn);
  caml_leave_blocking_section();
  caml_remove_global_root(&_dst);
  caml_remove_global_root(&_src);

  return Val_unit;
}

CAMLprim value caml_rgb_bilinear_scale(value _dst, value _src, value xscale, value yscale)
{
  frame src,dst;
  frame_of_value(_src, &src);
  frame_of_value(_dst, &dst);
  int i, j, c, i2, j2;
  float ax = Double_val(xscale),
        ay = Double_val(yscale);
  int ox = (dst.width - src.width * ax) / 2,
      oy = (dst.height - src.height * ay) / 2;
  float dx, dy;

  assert(ox >= 0 && oy >= 0);

  caml_register_global_root(&_dst);
  caml_register_global_root(&_src);
  caml_enter_blocking_section();
  if (ox != 0 || oy != 0)
    rgb_blank(&dst);
  for (j = oy; j < dst.height - oy; j++)
    for (i = ox; i < dst.width - ox; i++)
    {
      dx = (i - ox) / ax;
      i2 = floorl(dx);
      dx -= i2;
      dy = (j - oy) / ay;
      j2 = floorl(dy);
      dy -= j2;
      for (c = 0; c < Rgb_elems_per_pixel; c++)
        Color(&dst, c, i, j) =
          CLIP((int)
              ((Space_clip_color(&src, c, i2, j2) * (1-dx) * (1-dy)) +
              (Space_clip_color(&src, c, i2+1, j2) * dx * (1-dy)) +
              (Space_clip_color(&src, c, i2, j2+1) * (1-dx) * dy) +
              (Space_clip_color(&src, c, i2+1, j2+1) * dx * dy)));
    }
  caml_leave_blocking_section();
  caml_remove_global_root(&_dst);
  caml_remove_global_root(&_src);

  return Val_unit;
}

/*
CAMLprim value caml_rgb_scale(value _dst, value _src)
{
  frame src,dst;
  frame_of_value(_src, &src);
  frame_of_value(_dst, &dst);

  int i, j, c;

  caml_register_global_root(&_dst);
  caml_register_global_root(&_src);
  caml_enter_blocking_section();
  for (j = 0; j < dst.height; j++)
    for (i = 0; i < dst.width; i++)
      for (c = 0; c < Rgb_elems_per_pixel; c++)
        Color(&dst, c, i, j) = Color(&src, c, i * src.width / dst.width, j * src.height / dst.height);
  caml_leave_blocking_section();
  caml_remove_global_root(&_dst);
  caml_remove_global_root(&_src);

  return Val_unit;
}

CAMLprim value caml_rgb_proportional_scale(value _dst, value _src)
{
  frame src,dst;
  frame_of_value(_src, &src);
  frame_of_value(_dst, &dst);

  int i, j, c;
  int cn, cd, ox, oy;

  if (dst.height * src.width < src.height * dst.width)
  {
    cn = dst.height;
    cd = src.height;
    ox = (dst.width - src.width * cn / cd) / 2;
    oy = 0;
  }
  else
  {
    cn = dst.width;
    cd = src.width;
    ox = 0;
    oy = (dst.height - src.height * cn / cd) / 2;
  }

  caml_register_global_root(&_dst);
  caml_register_global_root(&_src);
  caml_enter_blocking_section();
  rgb_blank(&dst);
  for (j = oy; j < dst.height - oy; j++)
    for (i = ox; i < dst.width - ox; i++)
      for (c = 0; c < Rgb_elems_per_pixel; c++)
        Color(&dst, c, i, j) = Color(&src, c, (i - ox) * cd / cn, (j - oy) * cd / cn);
  caml_leave_blocking_section();
  caml_remove_global_root(&_dst);
  caml_remove_global_root(&_src);

  return Val_unit;
}
*/

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
  frame rgb;
  frame_of_value(_rgb,&rgb);
  int len = Rgb_num_pix(&rgb);
  char *bmp = malloc(54 + 3 * len);
  int i, j;
  unsigned char a;

  caml_register_global_root(&_rgb);
  caml_enter_blocking_section();
  bmp[0]='B';                       /* Magic number */
  bmp[1]='M';
  bmp_pint32(bmp+2 , 54 + 3 * len); /* File size */
  bmp_pint16(bmp+6 , 0);            /* Reserved */
  bmp_pint16(bmp+8 , 0);            /* Reserved */
  bmp_pint32(bmp+10, 54);           /* Data offset */
  bmp_pint32(bmp+14, 40);           /* Second header size */
  bmp_pint32(bmp+18, rgb.width);   /* Width */
  bmp_pint32(bmp+22, rgb.height);  /* Height */
  bmp_pint16(bmp+26, 1);            /* Nb of color planes */
  bmp_pint16(bmp+28, 24);           /* BPP */
  bmp_pint32(bmp+30, 0);            /* Compression */
  bmp_pint32(bmp+34, 3 * len);      /* Image size */
  bmp_pint32(bmp+38, 2834);         /* Horizontal resolution */
  bmp_pint32(bmp+42, 2834);         /* Vertical resolution */
  bmp_pint32(bmp+46, 0);            /* Number of colors */
  bmp_pint32(bmp+50, 0);            /* Number of important colors */

  for(j = 0; j < rgb.height; j++)
    for(i = 0; i < rgb.width; i++)
    {
      a = Alpha(&rgb, i, j);
      bmp[3 * ((rgb.height - j - 1) * rgb.width + i) + 0 + 54] = Blue(&rgb, i, j) * a / 0xff;
      bmp[3 * ((rgb.height - j - 1) * rgb.width + i) + 1 + 54] = Green(&rgb, i, j) * a / 0xff;
      bmp[3 * ((rgb.height - j - 1) * rgb.width + i) + 2 + 54] = Red(&rgb, i, j) * a / 0xff;
    }
  caml_leave_blocking_section();
  caml_remove_global_root(&_rgb);

  ans = caml_alloc_string(54 + 3 * len);
  memcpy(String_val(ans), bmp, 54 + 3 * len);
  free(bmp);

  CAMLreturn(ans);
}

CAMLprim value caml_rgb_to_color_array(value _rgb)
{
  CAMLparam1(_rgb);
  CAMLlocal2(ans, line);
  frame rgb;
  frame_of_value(_rgb,&rgb);

  int i, j, c;
  unsigned char a;

  ans = caml_alloc_tuple(rgb.height);
  for(j=0; j < rgb.height; j++)
  {
    line = caml_alloc_tuple(rgb.width);
    for(i=0; i < rgb.width; i++)
    {
      a = Alpha(&rgb, i, j);
      c = ((Red(&rgb,i,j) * a / 0xff) << 16)
        + ((Green(&rgb,i,j) * a / 0xff) << 8)
        + (Blue(&rgb,i,j) * a / 0xff);
      Store_field(line, i, Val_int(c));
    }
    Store_field(ans, j, line);
  }

  CAMLreturn(ans);
}

CAMLprim value caml_rgb_greyscale(value _rgb, value _sepia)
{
  frame rgb;
  frame_of_value(_rgb,&rgb);

  int sepia = Bool_val(_sepia);
  int i,j;
  unsigned char c;

  caml_register_global_root(&_rgb);
  caml_enter_blocking_section();
  for (j = 0; j < rgb.height; j++)
    for (i = 0; i < rgb.width; i++)
    {
      c = (Red(&rgb,i,j)
        +  Green(&rgb,i,j)
        +  Blue(&rgb,i,j)) / 3;
      if (sepia)
      {
        Red(&rgb,i,j)   = c;
        Green(&rgb,i,j) = c * 201 / 0xff;
        Blue(&rgb,i,j)  = c * 158 / 0xff;
      }
      else
      {
        Red(&rgb,i,j)   = c;
        Green(&rgb,i,j) = c;
        Blue(&rgb,i,j)  = c;
      }
    }
  caml_leave_blocking_section();
  caml_remove_global_root(&_rgb);

  return Val_unit;
}

CAMLprim value caml_rgb_add(value _dst, value _src)
{
  frame src,dst;
  frame_of_value(_src, &src);
  frame_of_value(_dst, &dst);

  int i, j, c;
  unsigned char sa;

  assert_same_dim(&src, &dst);
  caml_register_global_root(&_dst);
  caml_register_global_root(&_src);
  caml_enter_blocking_section();
  for (j = 0; j < dst.height; j++)
    for (i = 0; i < dst.width; i++)
    {
      sa = Alpha(&src,i,j);
      if (sa != 0)
      {
        if (sa == 0xff)
        {
          for (c = 0; c < Rgb_colors; c++)
            Color(&dst, c, i, j) = Color(&src, c, i, j);
          Alpha(&dst, i, j) = 0xff;
        }
        else
        {
          for (c = 0; c < Rgb_colors; c++)
            Color(&dst, c, i, j) = CLIP(Color(&src, c, i, j) * sa / 0xff + Color(&dst, c, i, j) * (0xff - sa) / 0xff);
          Alpha(&dst, i, j) = CLIP(sa + (0xff - sa) * Alpha(&dst, i, j));
        }
      }
    }
  caml_leave_blocking_section();
  caml_remove_global_root(&_dst);
  caml_remove_global_root(&_src);

  return Val_unit;
}

CAMLprim value caml_rgb_invert(value _rgb)
{
  frame rgb;
  frame_of_value(_rgb,&rgb);

  caml_register_global_root(&_rgb);
  caml_enter_blocking_section();
#ifdef HAVE_MMX
  /* See http://www.codeproject.com/KB/recipes/mmxintro.aspx?display=Print
   *     http://msdn.microsoft.com/en-us/library/698bxz2w(VS.80).aspx */
  unsigned char a1, a2;
  int i,j;
  __m64 *data = (__m64*)rgb.data;
  __m64 tmp;
  _mm_empty();
  __m64 f = _mm_set_pi8(0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff);
  for (j = 0; j < rgb.height; j ++)
    for (i = 0; i < rgb.width/2; i++)
    {
      tmp = _mm_subs_pu8(f, *data);
      a1 = rgb.data[3];
      a2 = rgb.data[7];
      *data = tmp;
      rgb.data[3] = a1;
      rgb.data[7] = a2;
      if (2 * i == rgb.width - 1)
        data += rgb.stride - 4 * rgb.width + 1;
      else
        data += 8;
    }
  _mm_empty();
#else
  int i, j, c;
  for (j = 0; j < rgb.height; j++)
    for (i = 0; i < rgb.width; i++)
      for (c = 0; c < Rgb_colors; c++)
        Color(&rgb, c, i, j) = 0xff - Color(&rgb, c, i, j);
#endif
  caml_leave_blocking_section();
  caml_remove_global_root(&_rgb);

  return Val_unit;
}

#define SO_PREC 0x10000
CAMLprim value caml_rgb_scale_opacity(value _rgb, value _x)
{
  frame rgb;
  frame_of_value(_rgb,&rgb);
  int x = Double_val(_x) * SO_PREC;
  int i, j;

  caml_register_global_root(&_rgb);
  caml_enter_blocking_section();
  for (j = 0; j < rgb.height; j++)
    for (i = 0; i < rgb.width; i++)
      Alpha(&rgb, i, j) = CLIP(Alpha(&rgb, i, j) * x / SO_PREC);
  caml_leave_blocking_section();
  caml_remove_global_root(&_rgb);

  return Val_unit;
}

CAMLprim value caml_rgb_disk_opacity(value _rgb, value _x, value _y, value _r)
{
  frame rgb;
  frame_of_value(_rgb,&rgb);
  int x = Int_val(_x);
  int y = Int_val(_y);
  int radius = Int_val(_r);
  int i, j, r;

  caml_register_global_root(&_rgb);
  caml_enter_blocking_section();
  for (j = 0; j < rgb.height; j++)
    for (i = 0; i < rgb.width; i++)
    {
      r = sqrtl((i - x) * (i - x) + (j - y) * (j - y));
      if (r > radius)
        Alpha(&rgb, i, j) = 0;
    }
  caml_leave_blocking_section();
  caml_remove_global_root(&_rgb);

  return Val_unit;
}

CAMLprim value caml_rgb_rotate(value _rgb, value _angle)
{
  frame rgb;
  frame_of_value(_rgb,&rgb);
  frame old;
  rgb_copy(&rgb,&old);
  double a = Double_val(_angle);
  int ox = rgb.width / 2,
      oy = rgb.height / 2;
  int i, j, c,
      i2, j2;

  caml_register_global_root(&_rgb);
  caml_enter_blocking_section();
  for (j = 0; j < rgb.height; j++)
    for (i = 0; i < rgb.width; i++)
    {
      i2 = (i - ox) * cos(a) + (j - oy) * sin(a) + ox;
      j2 = -(i - ox) * sin(a) + (j - oy) * cos(a) + oy;
      if (!Is_outside(&old, i2, j2))
        for (c = 0; c < Rgb_elems_per_pixel; c++)
          Color(&rgb, c, i, j) = Color(&old, c, i2, j2);
      else
        Alpha(&rgb, i, j) = 0;
    }
  caml_leave_blocking_section();
  caml_remove_global_root(&_rgb);

  rgb_free(&old);
  return Val_unit;
}

CAMLprim value caml_rgb_affine(value _rgb, value _ax, value _ay, value _ox, value _oy)
{
  frame rgb;
  frame_of_value(_rgb,&rgb);
  frame old;
  rgb_copy(&rgb,&old);
  double ax = Double_val(_ax),
         ay = Double_val(_ay);
  int i, j, i2, j2, c;
  int ox = Int_val(_ox),
      oy = Int_val(_oy);
  int dx = rgb.width / 2,  /* Center of scaling */
      dy = rgb.height / 2;
  int istart = max(0, (ox - dx) * ax + dx),
      iend = min(rgb.width, (rgb.width + ox + dx) * ax + dx),
      jstart = max(0, (oy - dy) * ay + dy),
      jend = min(rgb.height, (rgb.height + ox + dx) * ax + dx);

  caml_register_global_root(&_rgb);
  caml_enter_blocking_section();
  rgb_blank(&rgb);
  for (j = jstart; j < jend; j++)
    for (i = istart; i < iend; i++)
    {
      i2 = (i - dx) / ax + dx - ox;
      j2 = (j - dy) / ay + dy - oy;
      /* TODO: this test shouldn't be needed */
      if (!Is_outside(&old, i2, j2))
        for (c = 0; c < Rgb_elems_per_pixel; c++)
          Color(&rgb, c, i, j) = Color(&old, c, i2, j2);
    }
  caml_leave_blocking_section();
  caml_remove_global_root(&_rgb);

  rgb_free(&old);
  return Val_unit;
}

CAMLprim value caml_rgb_mask(value _rgb, value _mask)
{
  frame rgb;
  frame_of_value(_rgb,&rgb);
  frame mask;
  frame_of_value(_mask,&mask);
  int i, j;

  assert_same_dim(&rgb, &mask);
  caml_register_global_root(&_rgb);
  caml_register_global_root(&_mask);
  caml_enter_blocking_section();
  for (j = 0; j < rgb.height; j++)
    for (i = 0; i < rgb.width; i++)
      Alpha(&rgb, i, j) =
        CLIP(sqrt(
              Red(&mask, i, j) * Red(&mask, i, j) +
              Green(&mask, i, j) * Green(&mask, i, j) +
              Blue(&mask, i, j) * Blue(&mask, i, j))) *
        Alpha(&mask, i, j) / 0xff;
  caml_leave_blocking_section();
  caml_remove_global_root(&_rgb);
  caml_remove_global_root(&_mask);

  return Val_unit;
}

CAMLprim value caml_rgb_lomo(value _rgb)
{
  frame rgb;
  frame_of_value(_rgb, &rgb);
  int i, j, c;

  caml_register_global_root(&_rgb);
  caml_enter_blocking_section();
  for (j = 0; j < rgb.height; j++)
    for (i = 0; i < rgb.width; i++)
      for (c = 0; c < Rgb_colors; c++)
        Color(&rgb, c, i, j) = CLIP((1 - cos(Color(&rgb, c, i, j) * 3.1416 / 255)) * 255);
  caml_leave_blocking_section();
  caml_remove_global_root(&_rgb);

  return Val_unit;
}

CAMLprim value caml_rgb_color_to_alpha(value _rgb, value color, value _prec)
{
  frame rgb;
  frame_of_value(_rgb, &rgb);
  int r = Int_val(Field(color, 0)),
      g = Int_val(Field(color, 1)),
      b = Int_val(Field(color, 2));
  int prec = Int_val(_prec);
  int i, j;

  caml_register_global_root(&_rgb);
  caml_enter_blocking_section();
  for (j = 0; j < rgb.height; j++)
    for (i = 0; i < rgb.width; i++)
      if (abs(Red(&rgb, i, j) - r) <= prec && abs(Green(&rgb, i, j) - g) <= prec && abs(Blue(&rgb, i, j) - b) <= prec)
        Alpha(&rgb, i, j) = 0;
  caml_leave_blocking_section();
  caml_remove_global_root(&_rgb);

  return Val_unit;
}

CAMLprim value caml_rgb_blur_alpha(value _rgb)
{
  frame rgb;
  frame_of_value(_rgb, &rgb);
  frame old;
  rgb_copy(&rgb,&old);
  int w = 1;
  int i, j, k, l;
  int a;

  caml_register_global_root(&_rgb);
  caml_enter_blocking_section();
  for (j = w; j < rgb.height - w; j++)
    for (i = w; i < rgb.width - w; i++)
    {
      a = 0;
      for (l = -w; l <= w; l++)
        for (k = -w; k <= w; k++)
          a += Alpha(&old, i+k, j+l);
      Alpha(&rgb, i, j) = a / ((2*w+1)*(2*w+1));
    }
  rgb_free(&old);
  caml_leave_blocking_section();
  caml_remove_global_root(&_rgb);

  return Val_unit;
}
