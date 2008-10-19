#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/custom.h>

#include <assert.h>
#include <sys/ioctl.h>
#include <asm/types.h>
#include <linux/videodev.h>
#include <sys/mman.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

/*
CAMLprim value caml_v4l_caps(value _fd)
{
  CAMLparam1(_fd);
  int fd = Int_val(_fd);
  struct v4l2_capability cap;

  ioctl(fd, VIDIOC_QUERYCAP, &cap);
}
*/

CAMLprim value caml_v4l_caps(value _fd)
{
  CAMLparam1(_fd);
  CAMLlocal1(ans);
  int fd = Int_val(_fd);
  struct video_capability cap;

  assert(!ioctl(fd, VIDIOCGCAP, &cap));

  /* TODO: type */
  ans = caml_alloc_tuple(7);
  Store_field(ans, 0, caml_copy_string(cap.name));
  //assert(cap.type & VIDEO_TYPE_CAMERA);
  Store_field(ans, 1, Int_val(cap.channels));
  Store_field(ans, 2, Int_val(cap.audios));
  Store_field(ans, 3, Int_val(cap.maxwidth));
  Store_field(ans, 4, Int_val(cap.maxheight));
  Store_field(ans, 5, Int_val(cap.minwidth));
  Store_field(ans, 6, Int_val(cap.minheight));

  CAMLreturn(ans);
}

CAMLprim value caml_v4l_init(value _fd)
{
  CAMLparam1(_fd);
  int fd = Int_val(_fd);
  struct video_picture pic;

  assert(!ioctl(fd, VIDIOCGPICT, &pic));
  pic.depth = 24;
  pic.palette = VIDEO_PALETTE_RGB24;
  assert(!ioctl(fd, VIDIOCSPICT, &pic));

  CAMLreturn(Val_unit);
}

CAMLprim value caml_v4l_get_dims(value _fd)
{
  CAMLparam1(_fd);
  CAMLlocal1(ans);
  int fd = Int_val(_fd);
  struct video_window win;

  assert(!ioctl(fd, VIDIOCGWIN, &win));

  ans = caml_alloc_tuple(2);
  Store_field(ans, 0, Val_int(win.width));
  Store_field(ans, 1, Val_int(win.height));

  CAMLreturn(ans);
}

CAMLprim value caml_v4l_capture(value _fd, value _width, value _height)
{
  CAMLparam1(_fd);
  CAMLlocal1(ans);
  int fd = Int_val(_fd);
  int width = Int_val(_width),
      height = Int_val(_height);
  int len = width * height * 3;
  char *buf;
  struct video_mmap mm;
  int frame = 0;

  caml_enter_blocking_section();
  /* TODO: mmap once for all */
  buf = mmap(0, len, PROT_READ, MAP_SHARED, fd, 0);
  mm.frame = frame;
  mm.height = height;
  mm.width = width;
  mm.format = VIDEO_PALETTE_RGB24;
  assert(!ioctl(fd, VIDIOCMCAPTURE, &mm));
  assert(!ioctl(fd, VIDIOCSYNC, &frame));
  caml_leave_blocking_section();

  ans = caml_alloc_string(len);
  memcpy(String_val(ans), buf, len);
  munmap(buf, len);

  CAMLreturn(ans);
}

/*
CAMLprim value caml_v4l_init(value _fd)
{
  int fd = Int_val(_fd);
  int ret;

  struct video_capability cap;
  assert(!ioctl(fd, VIDIOCGCAP, &cap));
  printf("Name: %s\n", cap.name);
  {
    if (ret = EINVAL)
      printf("Invalid spec\n");
    printf("driver : %s\n", cap.driver);
    perror(strerror(errno));
    assert(0);
  }
  assert(cap.capabilities & V4L2_CAP_VIDEO_CAPTURE);

  struct v4l2_format fmt;
  fmt.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  assert(!ioctl(fd, VIDIOC_G_FMT, &fmt));
  fmt.fmt.pix.pixelformat = V4L2_PIX_FMT_RGB24;
  assert(!ioctl(fd, VIDIOC_S_FMT, &fmt));

  return Val_unit;
}
*/

/*
CAMLprim value caml_v4l_get_format(value _fd)
{
  CAMLparam1(_fd);
  CAMLlocal1(ans);
  int fd = Int_val(_fd);
  struct v4l2_format fmt;

  fmt.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;

  assert(!ioctl(fd, VIDIOC_G_FMT, &fmt));

  ans = caml_alloc_tuple(ans);
  Store_field(ans, 0, Val_int(fmt.fmt.pix.width));
  Store_field(ans, 1, Val_int(fmt.fmt.pix.height));

  CAMLreturn(ans);
}
*/
