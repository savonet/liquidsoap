/*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************/

#include <ortp.h>
#include <assert.h>
#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

// oRTP needs some info about the audio format it deals with
#define CHANNELS 2
#define SAMPLE_FREQ 44100
#define SAMPLE_SIZE 16
#define BITS_PER_SAMPLE (CHANNELS*SAMPLE_SIZE)
#define BYTES_PER_BUFFER 4096
#define BITS_PER_BUFFER (8*BYTES_PER_BUFFER)
#define TICKS_PER_BUFFER (BITS_PER_BUFFER/BITS_PER_SAMPLE)

static char zero_pattern = '\0';

static PayloadType liq_wav = {
  type            : PAYLOAD_AUDIO_CONTINUOUS,
  clock_rate      : SAMPLE_FREQ,
  bits_per_sample : BITS_PER_SAMPLE,
  zero_pattern    : &zero_pattern,
  pattern_length  : 1,
  normal_bitrate  : (SAMPLE_FREQ*BITS_PER_SAMPLE),
  mime_type       : "WAV"
} ;

static PayloadType liq_meta = {
  type            : PAYLOAD_AUDIO_PACKETIZED,
  clock_rate      : SAMPLE_FREQ,
  bits_per_sample : 0,
  zero_pattern    : NULL,
  pattern_length  : 0,
  normal_bitrate  : 0,
  mime_type       : "TEXT"
} ;

static RtpProfile liq_profile = {
  name : "liquidsoap profile",
  payload : { &liq_wav, &liq_meta }
} ;

static inline RtpSession* session_of_block(value block)
{
  return (RtpSession*)Field(block, 1);
}

static inline RtpSession* meta_session_of_block(value block)
{
  return (RtpSession*)Field(block, 2);
}

static inline uint32* timestamp_of_block(value block)
{
  return (uint32*)Field(block, 3);
}

static void finalize_session(value block)
{
  RtpSession* s = session_of_block(block);
  rtp_session_destroy(s);
  s = meta_session_of_block(block);
  rtp_session_destroy(s);
  free(timestamp_of_block(block));
}

CAMLprim value liquidsoap_new_session(value _mode, value _ip,
	                                  value _port, value _ttl) {
  CAMLparam4(_mode, _ip, _port, _ttl);

  CAMLlocal1(block);          // Where to store the data in the Caml side
  RtpSession* s;              // The audio session
  RtpSession* sm;             // The metadata session
  uint32* ts ;
  int mode = Int_val(_mode);
  char* ip = String_val(_ip);
  int port = Int_val(_port);
  int ttl = Int_val(_ttl);
  struct ip_mreqn mreq;       // For setsockopt

  ortp_init();
  assert (s = rtp_session_new(mode));
  assert (sm = rtp_session_new(mode));

  rtp_session_set_profile(s,&liq_profile);
  rtp_session_set_payload_type(s,0);

  rtp_session_set_profile(sm,&liq_profile);
  rtp_session_set_payload_type(sm,1);

  /* It is very likely that at least one listener will be one the same host
   * as the sender.
   * This is allowed thanks to the option IP_MULTICAST_LOOP
   */

  if (mode == RTP_SESSION_SENDONLY) {
    assert(0 == rtp_session_set_remote_addr(s,ip,port));
    assert(0 == rtp_session_set_remote_addr(sm,ip,port+2));

    /* TTL settings */
    if (inet_aton(ip, &mreq.imr_multiaddr) &&
        IN_MULTICAST(ntohl(mreq.imr_multiaddr.s_addr))) {

      if (setsockopt(s->rtp.socket,SOL_IP,
            IP_MULTICAST_TTL,&ttl,sizeof(ttl))) {
        perror("setsockopt rtp TTL");
        exit(-1);
      }
//      if (setsockopt(s->rtcp.socket,SOL_IP,
//            IP_MULTICAST_TTL,&ttl,sizeof(ttl))) {
//        perror("setsockopt rtcp TTL");
//        exit(-1);
//      }
      if (setsockopt(sm->rtp.socket,SOL_IP,
            IP_MULTICAST_TTL,&ttl,sizeof(ttl))) {
        perror("setsockopt meta rtp TTL");
        exit(-1);
      }
//      if (setsockopt(sm->rtcp.socket,SOL_IP,
//            IP_MULTICAST_TTL,&ttl,sizeof(ttl))) {
//        perror("setsockopt meta rtcp TTL");
//        exit(-1);
//      }
    }

  }else{
    assert(mode == RTP_SESSION_RECVONLY);
    assert(0 == rtp_session_set_local_addr(s,ip,port));
    assert(0 == rtp_session_set_remote_addr(s,"127.0.0.1",port));

    assert(0 == rtp_session_set_local_addr(sm,ip,port+2));
    assert(0 == rtp_session_set_remote_addr(sm,"127.0.0.1",port+2));

    rtp_session_set_jitter_compensation(sm,0);
  }

  // Prepare the timestamp
  ts = (uint32*)malloc(sizeof(uint32));
  *ts = 0 ;

  // Put the important data in a Caml value, and return it
  block = alloc_final(4, finalize_session, 150, 1000);
  Field(block, 1) = (value)s;
  Field(block, 2) = (value)sm;
  Field(block, 3) = (value)ts;
  CAMLreturn(block);
}


/* We only provide some functions for send and receiving frames.
 * Since oRTP cannot send or receive big buffers, we have to split
 * the liquidsoap frames.
 */

CAMLprim value liquidsoap_send_buffer(value _session, value _buffer) {
  CAMLparam2(_session, _buffer);
  RtpSession* session = session_of_block(_session);
  uint32* ts = timestamp_of_block(_session);
  int n ;

  n = rtp_session_send_with_ts(session,
      (unsigned char*)String_val(_buffer),
      BYTES_PER_BUFFER,
      *ts);
#ifdef DEBUG
  fprintf(stderr,"%9d Sent [%02x] %d bytes\n",*ts,
      (unsigned char)String_val(_buffer)[0],n);
#endif

  *ts += TICKS_PER_BUFFER ;

  CAMLreturn(Val_int(n));
}

CAMLprim value liquidsoap_recv_buffer(value _session, value _buffer) {
  CAMLparam2(_session, _buffer);
  int hm = 1 ;
  int err ;
  RtpSession* s = session_of_block(_session);
  uint32* ts = timestamp_of_block(_session);

  err = rtp_session_recv_with_ts(s,
      (unsigned char*)String_val(_buffer),
      BYTES_PER_BUFFER,
      *ts,&hm);
#ifdef DEBUG
  fprintf(stderr,"%9d Recv [%02x] %d\n", *ts,
      (unsigned char)String_val(_buffer)[0], err);
#endif
  if (err<=0) {
    hm = -1 ;
  } else {
    *ts += TICKS_PER_BUFFER ;
  }

  CAMLreturn(Val_int(hm));
}

/* The metadatas are sent as a string, parsing being done in the OCaml side.
 * Sending is trivial.
 * Receiving is done by receiving successive META_BS bytes chunks.
 */

CAMLprim value
liquidsoap_send_metadata(value _session, value _string)
{
  CAMLparam2(_session, _string);
  RtpSession* s = meta_session_of_block(_session);
  uint32 ts = *timestamp_of_block(_session);
  int size = caml_string_length(_string);

  int err = rtp_session_send_with_ts(s,(unsigned char*)String_val(_string),size,ts);

#ifdef DEBUG
  fprintf(stderr,"%9d Metablock sent %d\n", ts, err);
#endif
  CAMLreturn(Val_int(err));
}

#define META_BS 128
#define copy_buffer(b,len,dst) \
  dst = alloc_string(len); \
  memmove(String_val(dst), b, len);

CAMLprim value
liquidsoap_recv_metadata(value _session)
{
  CAMLparam1(_session);
  RtpSession* s = meta_session_of_block(_session);
  uint32 ts = *timestamp_of_block(_session);
  char* block = NULL;
  int hm=1;
  int offset=0;
  int allocated=0;
  int err = 0 ;
  CAMLlocal1(ret);

  while(hm) {
    if (offset==allocated) {
      allocated+=META_BS;
      assert (block = (char*)realloc((void*)block,allocated*sizeof(char)));
    }
    err = rtp_session_recv_with_ts(s,(unsigned char*)(block+offset),allocated-offset,ts,&hm);
    if (err>0) offset+=err;
    assert(err>=0);
#ifdef DEBUG
    fprintf(stderr,"%9d Metablock recv %d %d -> %d\n", ts, err, hm, offset);
#endif
  }

  copy_buffer(block,offset,ret);
  free(block);
  CAMLreturn(ret);
}
