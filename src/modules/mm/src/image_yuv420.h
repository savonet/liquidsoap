#define YUV420_y(v) (Caml_ba_data_val(Field(v, 0)))
#define YUV420_y_stride(v) (Int_val(Field(v, 1)))
#define YUV420_u(v) (Caml_ba_data_val(Field(v, 2)))
#define YUV420_v(v) (Caml_ba_data_val(Field(v, 3)))
#define YUV420_uv_stride(v) (Int_val(Field(v, 4)))
#define YUV420_width(v) (Int_val(Field(v, 5)))
#define YUV420_height(v) (Int_val(Field(v, 6)))
#define YUV420_alpha(v)                                                        \
  (Is_block(Field(v, 7)) ? Caml_ba_data_val(Field(Field(v, 7), 0)) : NULL)

typedef struct {
  int width;
  int height;
  unsigned char *y;
  int y_stride;
  unsigned char *u;
  unsigned char *v;
  int uv_stride;
  unsigned char *alpha;
} yuv420;

static void yuv420_of_value(yuv420 *yuv, value v) {
  yuv->y = YUV420_y(v);
  yuv->y_stride = YUV420_y_stride(v);
  yuv->u = YUV420_u(v);
  yuv->v = YUV420_v(v);
  yuv->uv_stride = YUV420_uv_stride(v);
  yuv->width = YUV420_width(v);
  yuv->height = YUV420_height(v);
  yuv->alpha = YUV420_alpha(v);
}

#define Y(yuv, i, j) yuv.y[j * yuv.y_stride + i]
#define U2(yuv, i, j) yuv.u[j * yuv.uv_stride + i]
#define V2(yuv, i, j) yuv.v[j * yuv.uv_stride + i]
#define U(yuv, i, j) U2(yuv, i / 2, j / 2)
#define V(yuv, i, j) V2(yuv, i / 2, j / 2)
#define A(yuv, i, j) yuv.alpha[j * yuv.y_stride + i]
