#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#include <sys/soundcard.h>
#include <sys/ioctl.h>
#include <assert.h>

CAMLprim value caml_oss_dsp_setfmt(value fd, value fmt)
{
  int f = Int_val(fmt);
  int ret = ioctl(Int_val(fd), SNDCTL_DSP_SETFMT, &f);

  /* TODO: raise errors */
  /* TODO: use format constants */
  assert(ret != -1);

  return Val_int(f);
}

CAMLprim value caml_oss_dsp_channels(value fd, value chans)
{
  int c = Int_val(chans);
  int ret = ioctl(Int_val(fd), SNDCTL_DSP_CHANNELS, &c);

  assert(ret != -1);

  return Val_int(c);
}

CAMLprim value caml_oss_dsp_speed(value fd, value speed)
{
  int s = Int_val(speed);
  int ret = ioctl(Int_val(fd), SNDCTL_DSP_SPEED, &s);

  assert(ret != -1);

  return Val_int(s);
}
