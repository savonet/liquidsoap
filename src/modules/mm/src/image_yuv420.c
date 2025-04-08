#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#include <assert.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

#include "image_pixel.h"
#include "image_rgb.h"
#include "image_yuv420.h"

#define max(a, b) (a > b ? a : b)
#define min(a, b) (a < b ? a : b)

CAMLprim value caml_yuv420_fill(value img, value p, value uv_height) {
  CAMLparam2(img, p);
  int y = Int_val(Field(p, 0));
  int u = Int_val(Field(p, 1));
  int v = Int_val(Field(p, 2));
  int height = YUV420_height(img);
  int y_stride = YUV420_y_stride(img);
  int uv_stride = YUV420_uv_stride(img);
  memset(YUV420_y(img), y, height * y_stride);
  memset(YUV420_u(img), u, Int_val(uv_height) * uv_stride);
  memset(YUV420_v(img), v, Int_val(uv_height) * uv_stride);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv420_to_int_image(value img) {
  CAMLparam1(img);
  CAMLlocal2(ans, tmp);
  yuv420 yuv;
  yuv420_of_value(&yuv, img);
  int i, j;
  int y, u, v, r, g, b, a;
  ans = caml_alloc_tuple(yuv.height);
  for (j = 0; j < yuv.height; j++) {
    tmp = caml_alloc_tuple(yuv.width);
    for (i = 0; i < yuv.width; i++) {
      y = Y(yuv, i, j);
      u = U(yuv, i, j);
      v = V(yuv, i, j);
      r = RofYUV(y, u, v);
      g = GofYUV(y, u, v);
      b = BofYUV(y, u, v);
      if (yuv.alpha) {
        a = A(yuv, i, j);
        r = r * a / 0xff;
        g = g * a / 0xff;
        b = b * a / 0xff;
      }
      Store_field(tmp, i, Val_int((r << 16) + (g << 8) + b));
    }
    Store_field(ans, j, tmp);
  }
  CAMLreturn(ans);
}

CAMLprim value caml_yuv420_of_rgb24_string(value img, value s) {
  CAMLparam2(img, s);
  yuv420 yuv;
  yuv420_of_value(&yuv, img);
  // We don't copy so we cannot release the lock
  unsigned char *data = (unsigned char *)String_val(s);
  int i, j;

  for (j = 0; j < yuv.height; j++)
    for (i = 0; i < yuv.width; i++) {
      int r = data[3 * (j * yuv.width + i) + 0];
      int g = data[3 * (j * yuv.width + i) + 1];
      int b = data[3 * (j * yuv.width + i) + 2];
      Y(yuv, i, j) = YofRGB(r, g, b);
      if (i % 2 == 0 && j % 2 == 0) {
        U(yuv, i, j) = UofRGB(r, g, b);
        V(yuv, i, j) = VofRGB(r, g, b);
      }
    }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv420_of_rgba32(value _rgb, value img) {
  CAMLparam2(_rgb, img);
  frame rgb;
  frame_of_value(_rgb, &rgb);
  yuv420 yuv;
  yuv420_of_value(&yuv, img);
  int i, j;

  caml_enter_blocking_section();
  for (j = 0; j < rgb.height; j++)
    for (i = 0; i < rgb.width; i++) {
      int r = Red(&rgb, i, j);
      int g = Green(&rgb, i, j);
      int b = Blue(&rgb, i, j);
      Y(yuv, i, j) = YofRGB(r, g, b);
      A(yuv, i, j) = Alpha(&rgb, i, j);
      if (i % 2 == 0 && j % 2 == 0) {
        U(yuv, i, j) = UofRGB(r, g, b);
        V(yuv, i, j) = VofRGB(r, g, b);
      }
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv420_to_rgba32(value img, value _rgb) {
  CAMLparam2(img, _rgb);
  frame rgb;
  frame_of_value(_rgb, &rgb);
  yuv420 yuv;
  yuv420_of_value(&yuv, img);
  int i, j;

  caml_enter_blocking_section();
  for (j = 0; j < yuv.height; j++)
    for (i = 0; i < yuv.width; i++) {
      int y = Y(yuv, i, j);
      int u = U(yuv, i, j);
      int v = V(yuv, i, j);
      Red(&rgb, i, j) = RofYUV(y, u, v);
      Green(&rgb, i, j) = GofYUV(y, u, v);
      Blue(&rgb, i, j) = BofYUV(y, u, v);
      Alpha(&rgb, i, j) = yuv.alpha ? A(yuv, i, j) : 0xff;
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv420_scale(value _src, value _dst) {
  CAMLparam2(_src, _dst);
  yuv420 src, dst;
  yuv420_of_value(&src, _src);
  yuv420_of_value(&dst, _dst);
  int i, j, is, js;

  assert(!src.alpha || dst.alpha);

  caml_enter_blocking_section();
  /*
  for (j = 0; j < dst.height; j++)
    for (i = 0; i < dst.width; i++) {
      is = i * src.width / dst.width;
      js = j * src.height / dst.height;
      Y(dst, i, j) = Y(src, is, js);
      U(dst, i, j) = U(src, is, js);
      V(dst, i, j) = V(src, is, js);
    }
  */
  for (j = 0; j < dst.height; j++)
    for (i = 0; i < dst.width; i++) {
      is = i * src.width / dst.width;
      js = j * src.height / dst.height;
      Y(dst, i, j) = Y(src, is, js);
    }
  for (j = 0; j < dst.height / 2; j++)
    for (i = 0; i < dst.width / 2; i++) {
      is = i * (src.width / 2) / (dst.width / 2);
      js = j * (src.height / 2) / (dst.height / 2);
      U2(dst, i, j) = U2(src, is, js);
      V2(dst, i, j) = V2(src, is, js);
    }
  if (src.alpha)
    for (j = 0; j < dst.height; j++)
      for (i = 0; i < dst.width; i++) {
        is = i * src.width / dst.width;
        js = j * src.height / dst.height;
        A(dst, i, j) = A(src, is, js);
      }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv420_scale_coef(value _src, value _dst, value xscale,
                                      value yscale) {
  CAMLparam4(_src, _dst, xscale, yscale);
  yuv420 src, dst;
  yuv420_of_value(&src, _src);
  yuv420_of_value(&dst, _dst);
  // x scaling (xn: numerator, xd: denominator)
  int xn = Int_val(Field(xscale, 0));
  int xd = Int_val(Field(xscale, 1));
  // y scaling
  int yn = Int_val(Field(yscale, 0));
  int yd = Int_val(Field(yscale, 1));
  // offsets
  int ox = (dst.width - src.width * xn / xd) / 2;
  int oy = (dst.height - src.height * yn / yd) / 2;
  int i, j;

  assert(ox >= 0 && oy >= 0);

  caml_enter_blocking_section();
  // TODO: blank
  /* if (ox != 0 || oy != 0) rgb_blank(&dst); */
  for (j = oy; j < dst.height - oy; j++)
    for (i = ox; i < dst.width - ox; i++) {
      int is = (i - ox) * xd / xn;
      int js = (j - oy) * yd / yn;
      Y(dst, i, j) = Y(src, is, js);
      if (i % 2 == 0 && j % 2 == 0) {
        U(dst, i, j) = U(src, is, js);
        V(dst, i, j) = V(src, is, js);
      }
      if (src.alpha)
        A(dst, i, j) = A(src, is, js);
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv420_add(value _src, value _x, value _y, value _dst) {
  CAMLparam4(_src, _x, _y, _dst);
  int x = Int_val(_x);
  int y = Int_val(_y);
  yuv420 src, dst;
  yuv420_of_value(&src, _src);
  yuv420_of_value(&dst, _dst);

  int i, j;
  // The portion of dst which will actually get modified
  int ia = max(x, 0);
  int ib = min(x + src.width, dst.width);
  int ja = max(y, 0);
  int jb = min(y + src.height, dst.height);

  if (!(ia < ib))
    CAMLreturn(Val_unit);

  caml_enter_blocking_section();
  if (src.alpha == NULL) {
    int il = ib - ia;
    for (j = ja; j < jb; j++) {
      /*
      for (i = ia; i < ib; i++) {
        int is = i - x;
        int js = j - y;
        Y(dst, i, j) = Y(src, is, js);
      }
      */
      int is = ia - x;
      int js = j - y;
      memcpy(dst.y + (j * dst.y_stride + ia), src.y + (js * src.y_stride + is),
             il);
    }
    /* U and V only have to be done once every two times */
    /*
    for (j = ja; j < jb; j+=2)
      for (i = ia; i < ib; i+=2) {
        int is = i - x;
        int js = j - y;
        U(dst, i, j) = U(src, is, js);
        V(dst, i, j) = V(src, is, js);
      }
    */
    for (j = ja; j < jb; j += 2) {
      int is = ia - x;
      int js = j - y;
      memcpy(dst.u + (j / 2 * dst.uv_stride + ia / 2),
             src.u + (js / 2 * src.uv_stride + is / 2), il / 2);
      memcpy(dst.v + (j / 2 * dst.uv_stride + ia / 2),
             src.v + (js / 2 * src.uv_stride + is / 2), il / 2);
    }
    if (dst.alpha)
      for (j = ja; j < jb; j++)
        /*
        for (i = ia; i < ib; i++)
          A(dst, i, j) = 0xff;
        */
        memset(dst.alpha + (j * dst.y_stride + ia), 0xff, il);
  }
  /*
  else if (src.alpha == NULL) {
    int il = ib - ia;
    for (j = ja; j < jb; j++) {
      for (i = ia; i < ib; i++) {
        int is = i - x;
        int js = j - y;
        Y(dst, i, j) = Y(src, is, js);
        U(dst, i, j) = U(src, is, js);
        V(dst, i, j) = V(src, is, js);
      }
    }
    if (dst.alpha)
      for (j = ja; j < jb; j++)
        for (i = ia; i < ib; i++)
          A(dst, i, j) = 0xff;
  }
  */
  else
    for (j = ja; j < jb; j++)
      for (i = ia; i < ib; i++) {
        int is = i - x;
        int js = j - y;
        int a = A(src, is, js);

        if (a == 0) {
        } else if (a == 0xff) {
          Y(dst, i, j) = Y(src, is, js);
          U(dst, i, j) = U(src, is, js);
          V(dst, i, j) = V(src, is, js);
          if (dst.alpha)
            A(dst, i, j) = 0xff;
        } else {
          Y(dst, i, j) =
              (Y(src, is, js) * a + Y(dst, i, j) * (0xff - a)) / 0xff;
          if (dst.alpha)
            A(dst, i, j) = 0xff - ((0xff - a) * (0xff - A(dst, i, j))) / 0xff;
          if (i % 2 == 0 && j % 2 == 0) {
            U(dst, i, j) =
                (U(src, is, js) * a + U(dst, i, j) * (0xff - a)) / 0xff;
            V(dst, i, j) =
                (V(src, is, js) * a + V(dst, i, j) * (0xff - a)) / 0xff;
          }
        }
      }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv420_get_pixel_rgba(value img, value _i, value _j) {
  CAMLparam3(img, _i, _j);
  CAMLlocal1(ans);
  yuv420 yuv;
  yuv420_of_value(&yuv, img);
  int i = Int_val(_i);
  int j = Int_val(_j);

  int y = Y(yuv, i, j);
  int u = U(yuv, i, j);
  int v = V(yuv, i, j);
  int a = yuv.alpha ? A(yuv, i, j) : 0xff;
  int r = RofYUV(y, u, v);
  int g = GofYUV(y, u, v);
  int b = BofYUV(y, u, v);

  ans = caml_alloc_tuple(4);
  Store_field(ans, 0, Val_int(r));
  Store_field(ans, 1, Val_int(g));
  Store_field(ans, 2, Val_int(b));
  Store_field(ans, 3, Val_int(a));
  CAMLreturn(ans);
}

CAMLprim value caml_yuv420_set_pixel_rgba(value img, value _i, value _j,
                                          value c) {
  CAMLparam4(img, _i, _j, c);
  yuv420 yuv;
  yuv420_of_value(&yuv, img);
  int i = Int_val(_i);
  int j = Int_val(_j);
  int r = Int_val(Field(c, 0));
  int g = Int_val(Field(c, 1));
  int b = Int_val(Field(c, 2));
  int a = Int_val(Field(c, 3));

  Y(yuv, i, j) = YofRGB(r, g, b);
  U(yuv, i, j) = UofRGB(r, g, b);
  V(yuv, i, j) = VofRGB(r, g, b);
  if (yuv.alpha)
    A(yuv, i, j) = a;

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv_randomize(value img) {
  CAMLparam1(img);
  yuv420 yuv;
  yuv420_of_value(&yuv, img);
  int i, j;

  caml_enter_blocking_section();
  for (j = 0; j < yuv.height; j++)
    for (i = 0; i < yuv.width; i++) {
      Y(yuv, i, j) = rand();
      U(yuv, i, j) = rand();
      V(yuv, i, j) = rand();
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv_greyscale(value img) {
  CAMLparam1(img);
  yuv420 yuv;
  yuv420_of_value(&yuv, img);
  int i, j;

  caml_enter_blocking_section();
  for (j = 0; j < yuv.height; j += 2)
    for (i = 0; i < yuv.width; i += 2) {
      U(yuv, i, j) = 0x7f;
      V(yuv, i, j) = 0x7f;
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

#define PIXEL_PRECISON 0x10000
CAMLprim value caml_yuv_scale_alpha(value img, value _c) {
  CAMLparam2(img, _c);
  yuv420 yuv;
  yuv420_of_value(&yuv, img);
  int c = Double_val(_c) * PIXEL_PRECISON;
  int amax = CLIP(0xff * c / PIXEL_PRECISON);
  int i, j;

  caml_enter_blocking_section();
  for (j = 0; j < yuv.height; j++)
    for (i = 0; i < yuv.width; i++) {
      int a = A(yuv, i, j);
      if (a != 0) {
        if (a == 0xff)
          A(yuv, i, j) = amax;
        else
          A(yuv, i, j) = CLIP(a * c / PIXEL_PRECISON);
      }
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv_disk_alpha(value img, value _x, value _y, value _r) {
  CAMLparam4(img, _x, _y, _r);
  yuv420 yuv;
  yuv420_of_value(&yuv, img);
  int x = Int_val(_x);
  int y = Int_val(_y);
  int radius = Int_val(_r);
  radius = radius * radius;
  int i, j;

  caml_enter_blocking_section();
  for (j = 0; j < yuv.height; j++)
    for (i = 0; i < yuv.width; i++) {
      int r = (i - x) * (i - x) + (j - y) * (j - y);
      if (r > radius)
        A(yuv, i, j) = 0;
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

#define crop(x, m) (x > m ? m : (x < 0 ? 0 : x))

CAMLprim value caml_yuv_box_alpha_native(value img, value _x, value _y,
                                         value _w, value _h, value _a) {
  CAMLparam1(img);
  yuv420 yuv;
  yuv420_of_value(&yuv, img);
  int x = crop(Int_val(_x), yuv.width);
  int y = crop(Int_val(_y), yuv.height);
  int w = crop(Int_val(_w), yuv.width);
  int h = max(Int_val(_h), yuv.height);
  int a = CLIP(Double_val(_a) * PIXEL_PRECISON);
  int i, j;

  caml_enter_blocking_section();
  for (j = y; j < h; j++)
    for (i = x; i < w; i++)
      A(yuv, i, j) = a;
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv_box_alpha_bytecode(value *argv, int argn) {
  return caml_yuv_box_alpha_native(argv[0], argv[1], argv[2], argv[3], argv[4],
                                   argv[5]);
}

CAMLprim value caml_yuv_alpha_of_color(value img, value _y, value _u, value _v,
                                       value _d) {
  CAMLparam5(img, _y, _u, _v, _d);
  yuv420 yuv;
  yuv420_of_value(&yuv, img);
  int y = Int_val(_y);
  int u = Int_val(_u);
  int v = Int_val(_v);
  int d = Int_val(_d);
  int i, j;
  int yy, uu, vv;

  d = d * d * 3;

  caml_enter_blocking_section();
  for (j = 0; j < yuv.height; j++)
    for (i = 0; i < yuv.width; i++) {
      yy = Y(yuv, i, j) - y;
      uu = U(yuv, i, j) - u;
      vv = V(yuv, i, j) - v;
      if (yy * yy + uu * uu + vv * vv <= d)
        A(yuv, i, j) = 0;
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv_rotate(value _src, value _ox, value _oy, value _angle,
                               value _dst) {
  CAMLparam5(_src, _ox, _oy, _angle, _dst);
  yuv420 src, dst;
  yuv420_of_value(&src, _src);
  yuv420_of_value(&dst, _dst);
  int ox = Int_val(_ox);
  int oy = Int_val(_oy);
  double a = Double_val(_angle);
  double sina = sin(a);
  double cosa = cos(a);
  int i, j, i2, j2;

  caml_enter_blocking_section();
  for (j = 0; j < dst.height; j++)
    for (i = 0; i < dst.width; i++) {
      i2 = (i - ox) * cosa + (j - oy) * sina + ox;
      j2 = -(i - ox) * sina + (j - oy) * cosa + oy;
      if (0 <= i2 && i2 < src.width && 0 <= j2 && j2 < src.height) {
        Y(dst, i, j) = Y(src, i2, j2);
        U(dst, i, j) = U(src, i2, j2);
        V(dst, i, j) = V(dst, i2, j2);
        A(dst, i, j) = src.alpha ? A(src, i2, j2) : 0xff;
      } else
        A(dst, i, j) = 0;
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv_gradient_uv(value _img, value uv, value duvx,
                                    value duvy) {
  CAMLparam4(_img, uv, duvx, duvy);
  yuv420 img;
  yuv420_of_value(&img, _img);
  int u = Int_val(Field(uv, 0));
  int v = Int_val(Field(uv, 1));
  int ux = Int_val(Field(duvx, 0)) - u;
  int vx = Int_val(Field(duvx, 1)) - v;
  int uy = Int_val(Field(duvy, 0)) - u;
  int vy = Int_val(Field(duvy, 1)) - v;

  int i, j;

  caml_enter_blocking_section();
  for (j = 0; j < img.height; j++)
    for (i = 0; i < img.width; i++) {
      Y(img, i, j) = 0xff;
      U(img, i, j) = u + ux * i / img.width + uy * j / img.height;
      V(img, i, j) = v + vx * i / img.width + vy * j / img.height;
      if (img.alpha)
        A(img, i, j) = 0xff;
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv_invert(value _img) {
  CAMLparam1(_img);
  yuv420 img;
  yuv420_of_value(&img, _img);

  int i, j;

  caml_enter_blocking_section();
  for (j = 0; j < img.height; j++)
    for (i = 0; i < img.width; i++) {
      Y(img, i, j) = 0xff - Y(img, i, j);
      U(img, i, j) = 0xff - U(img, i, j);
      V(img, i, j) = 0xff - V(img, i, j);
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv_sepia(value _img) {
  CAMLparam1(_img);
  yuv420 img;
  yuv420_of_value(&img, _img);

  int i, j;
  int y, u, v;
  int r, g, b;
  int c;

  caml_enter_blocking_section();
  for (j = 0; j < img.height; j++)
    for (i = 0; i < img.width; i++) {
      // TODO: avoid converting back and forth
      y = Y(img, i, j);
      u = U(img, i, j);
      v = V(img, i, j);
      r = RofYUV(y, u, v);
      g = GofYUV(y, u, v);
      b = BofYUV(y, u, v);
      c = (r + g + b) / 3;
      r = c;
      g = c * 201 / 0xff;
      b = c * 201 / 0xff;
      Y(img, i, j) = YofRGB(r, g, b);
      U(img, i, j) = UofRGB(r, g, b);
      V(img, i, j) = VofRGB(r, g, b);
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv_lomo(value _img) {
  CAMLparam1(_img);
  yuv420 img;
  yuv420_of_value(&img, _img);

  int i, j;

  caml_enter_blocking_section();
  for (j = 0; j < img.height; j++)
    for (i = 0; i < img.width; i++) {
      U(img, i, j) = V(img, i, j);
      V(img, i, j) = U(img, i, j);
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv_is_opaque(value _img) {
  CAMLparam1(_img);
  yuv420 img;
  yuv420_of_value(&img, _img);

  int i, j;
  int ans = 1;

  caml_enter_blocking_section();
  for (j = 0; j < img.height; j++) {
    if (!ans)
      break;
    for (i = 0; i < img.width; i++)
      if (A(img, i, j) != 0xff) {
        ans = 0;
        break;
      }
  }
  caml_leave_blocking_section();

  CAMLreturn(Val_bool(ans));
}

CAMLprim value caml_yuv_alpha_of_sameness(value _ref, value _img, value _d) {
  CAMLparam3(_ref, _img, _d);
  yuv420 ref, img;
  yuv420_of_value(&ref, _ref);
  yuv420_of_value(&img, _img);
  int d = Int_val(_d);

  int i, j;
  int y, u, v;

  d = d * d * 3;

  caml_enter_blocking_section();
  for (j = 0; j < img.height; j++)
    for (i = 0; i < img.width; i++) {
      {
        y = Y(img, i, j) - Y(ref, i, j);
        u = U(img, i, j) - U(ref, i, j);
        v = V(img, i, j) - V(ref, i, j);
        if (y * y + u * u + v * v <= d)
          A(img, i, j) = 0;
      }
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv_alpha_of_diff(value _ref, value _img, value _level,
                                      value _speed) {
  CAMLparam4(_ref, _img, _level, _speed);
  yuv420 ref, img;
  yuv420_of_value(&ref, _ref);
  yuv420_of_value(&img, _img);
  int level = Int_val(_level);
  int speed = Int_val(_speed);
  int i, j;
  int y, u, v;
  int d;

  if (speed < 1)
    speed = 1;
  level = level * level * 3;

  caml_enter_blocking_section();
  for (j = 0; j < img.height; j++)
    for (i = 0; i < img.width; i++) {
      {
        y = Y(img, i, j) - Y(ref, i, j);
        u = U(img, i, j) - U(ref, i, j);
        v = V(img, i, j) - V(ref, i, j);
        d = y * y + u * u + v * v;
        if (d <= level)
          A(img, i, j) =
              A(img, i, j) * (speed * level - (level - d)) / (speed * level);
        else
          d = level - min(level, d - level);
        A(img, i, j) = 0xff - (0xff - A(img, i, j)) *
                                  (speed * level - (level - d)) /
                                  (speed * level);
      }
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv_hmirror(value _img) {
  CAMLparam1(_img);
  yuv420 img;
  yuv420_of_value(&img, _img);
  int w = img.width;
  int i, j;
  int y, u, v;

  caml_enter_blocking_section();
  for (j = 0; j < img.height; j++) {
    for (i = 0; i < w / 2; i++) {
      y = Y(img, i, j);
      Y(img, i, j) = Y(img, w - 1 - i, j);
      Y(img, w - 1 - i, j) = y;
    }
  }
  for (j = 0; j < img.height / 2; j++)
    for (i = 0; i < w / 4; i++) {
      u = U2(img, i, j);
      U2(img, i, j) = U2(img, w / 2 - 1 - i, j);
      U2(img, w / 2 - 1 - i, j) = u;
      v = V2(img, i, j);
      V2(img, i, j) = V2(img, w / 2 - 1 - i, j);
      V2(img, w / 2 - 1 - i, j) = v;
    }

  if (img.alpha)
    for (j = 0; j < img.height; j++)
      for (i = 0; i < w / 2; i++)
        A(img, i, j) = A(img, w - i, j);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}
