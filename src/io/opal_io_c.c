#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

#include <opal.h>
#include <limits.h>
#include <unistd.h>
#include <assert.h>
#include <stdio.h>

#if defined(_WIN32)

  #include <windows.h>

  #ifdef _DEBUG
    #define OPAL_DLL "OPALd.DLL"
  #else
    #define OPAL_DLL "OPAL.DLL"
  #endif

  #define OPEN_LIBRARY(name)             LoadLibrary(name)
  #define GET_LIBRARY_FUNCTION(dll, fn)  GetProcAddress(dll, fn)

  HINSTANCE hDLL = NULL;

#else // _WIN32

  #include <memory.h>
  #include <dlfcn.h>

  #define OPAL_DLL "libopal.so"

  #define OPEN_LIBRARY(name)             dlopen(name, RTLD_NOW)
  #define GET_LIBRARY_FUNCTION(dll, fn)  dlsym(dll, (const char *)(fn));

  void * hDLL = NULL;

#endif // _WIN32

/* Obtained from dlopen */
OpalInitialiseFunction  InitialiseFunction;
OpalShutDownFunction    ShutDownFunction;
OpalGetMessageFunction  GetMessageFunction;
OpalSendMessageFunction SendMessageFunction;
OpalFreeMessageFunction FreeMessageFunction;

#define Handle_val(v) ((OpalHandle)v)

/* TODO: not a global value */
int write_fd[2];

CAMLprim value caml_opal_init(value unit)
{
  OpalHandle hOPAL;
  unsigned version;

  /* dlopen only once. */
  if (!hDLL)
  {
    assert(hDLL = OPEN_LIBRARY(OPAL_DLL));

    InitialiseFunction  = (OpalInitialiseFunction )GET_LIBRARY_FUNCTION(hDLL, OPAL_INITIALISE_FUNCTION  );
    ShutDownFunction    = (OpalShutDownFunction   )GET_LIBRARY_FUNCTION(hDLL, OPAL_SHUTDOWN_FUNCTION    );
    GetMessageFunction  = (OpalGetMessageFunction )GET_LIBRARY_FUNCTION(hDLL, OPAL_GET_MESSAGE_FUNCTION );
    SendMessageFunction = (OpalSendMessageFunction)GET_LIBRARY_FUNCTION(hDLL, OPAL_SEND_MESSAGE_FUNCTION);
    FreeMessageFunction = (OpalFreeMessageFunction)GET_LIBRARY_FUNCTION(hDLL, OPAL_FREE_MESSAGE_FUNCTION);

    assert(InitialiseFunction && ShutDownFunction && GetMessageFunction && SendMessageFunction && FreeMessageFunction);
  }

  version = OPAL_C_API_VERSION;
  caml_enter_blocking_section();
  hOPAL = InitialiseFunction(&version, OPAL_PREFIX_H323 " " OPAL_PREFIX_SIP " " OPAL_PREFIX_IAX2 " " OPAL_PREFIX_LOCAL " TraceLevel=4");
  caml_leave_blocking_section();
  assert(hOPAL);
  write_fd[0] = -1;
  write_fd[1] = -1;

  return (value)hOPAL;
}

CAMLprim value caml_opal_shutdown(value h)
{
  OpalHandle hOPAL = Handle_val(h);

  caml_enter_blocking_section();
  ShutDownFunction(hOPAL);
  caml_leave_blocking_section();

  return Val_unit;
}

/* This function is called in a blocking section! */
/* TODO: raise exceptions */
static OpalMessage *MySendCommand(OpalHandle hOPAL, OpalMessage *command)
{
  OpalMessage *response;

  caml_enter_blocking_section();
  response = SendMessageFunction(hOPAL, command);
  caml_leave_blocking_section();

  if (!response)
    return NULL;
  if (response->m_type != OpalIndCommandError)
    return response;

  if (response->m_param.m_commandError == NULL || *response->m_param.m_commandError == '\0')
    printf("OPAL error.\n");
  else
    printf("OPAL error: %s\n", response->m_param.m_commandError);

  FreeMessageFunction(response);

  return NULL;
}

CAMLprim value caml_opal_set_protocol_parameters(value h, value username, value displayname, value interface)
{
  /* TODO: product */
  /* TODO: blocking section */
  OpalHandle hOPAL = Handle_val(h);
  OpalMessage command;
  OpalMessage *response;

  memset(&command, 0, sizeof(command));
  command.m_type = OpalCmdSetProtocolParameters;

  command.m_param.m_protocol.m_userName = String_val(username);
  command.m_param.m_protocol.m_displayName = String_val(displayname);
  command.m_param.m_protocol.m_interfaceAddresses = String_val(interface);

  assert(response = MySendCommand(hOPAL, &command));
  FreeMessageFunction(response);

  return Val_unit;
}

static void write_int(int fd, int n)
{
  assert(write(fd, &n, sizeof(int)) == sizeof(int));
}

static int read_int(int fd)
{
  int n;

  assert(read(fd, &n, sizeof(int)) == sizeof(int));

  return n;
}

static void write_data(int fd, const char *s, int len)
{
  int wc = 0;
  int n;

  while (wc < len)
  {
    n = write(fd, s+wc, len-wc);
    assert(n > 0);
    wc += n;
  }
}

static void write_string(int fd, const char *s)
{
  int len = strlen(s)+1;

  write_int(fd, len);
  write_data(fd, s, len);
}

static void read_data(int fd, char *s, int len)
{
  int rc = 0;
  int n;

  while (rc < len)
  {
    n = read(fd, s+rc, len-rc);
    assert(n > 0);
    rc += n;
  }
}

static char* read_string(int fd)
{
  int len;
  char *s;

  len = read_int(fd);
  s = malloc(len);
  read_data(fd, s, len);
  return s;
}

static void write_packet(int fd, const char *token, const char *id, const char *format, void *data, int size)
{
  write_string(fd, token);
  write_string(fd, id);
  write_string(fd, format);
  write_int(fd, size);
  write_data(fd, data, size);
}

static int MyWriteMediaData(const char *token, const char *id, const char *format, void *userData, void *data, int size)
{
  if (size == 0)
    return 0;

  //printf("OPAL: writing %d bytes.\n", size);

  //len = write(write_fd, data, size);
  write_packet(write_fd[1], token, id, format, data, size);

  return size;
}

CAMLprim value caml_opal_read_data(value h)
{
  CAMLparam1(h);
  CAMLlocal1(ans);
  int fd = write_fd[0];
  int data_len;
  char *token, *id, *format, *data;

  /* TODO: fail if write callback is not used */
  assert(fd >= 0);

  caml_enter_blocking_section();
  token = read_string(fd);
  id = read_string(fd);
  format = read_string(fd);
  data_len = read_int(fd);
  data = malloc(data_len);
  read_data(fd, data, data_len);
  caml_leave_blocking_section();

  ans = caml_alloc_tuple(4);
  Store_field(ans, 0, caml_copy_string(token));
  Store_field(ans, 1, caml_copy_string(id));
  Store_field(ans, 2, caml_copy_string(format));
  Store_field(ans, 3, caml_alloc_string(data_len));
  memcpy(String_val(Field(ans, 3)), data, data_len);

  free(token);
  free(id);
  free(format);
  free(data);

  CAMLreturn(ans);
}

CAMLprim value caml_opal_set_general_parameters(value h, value auto_rx_media, value auto_tx_media, value write)
{
  OpalHandle hOPAL = Handle_val(h);
  OpalMessage command;
  OpalMessage *response;

  memset(&command, 0, sizeof(command));
  command.m_type = OpalCmdSetGeneralParameters;
  command.m_param.m_general.m_autoRxMedia = String_val(auto_rx_media);
  command.m_param.m_general.m_autoTxMedia = String_val(auto_tx_media);
  command.m_param.m_general.m_mediaDataHeader = OpalMediaDataPayloadOnly; /* TODO: param */

  if (Bool_val(write))
  {
    assert(!pipe(write_fd));
    command.m_param.m_general.m_mediaWriteData = MyWriteMediaData;
  }

  assert(response = MySendCommand(hOPAL, &command));
  FreeMessageFunction(response);

  return Val_unit;
}

#define Message_val(v) ((OpalMessage*)v)

/* TODO: finalizer */
static value val_message(OpalMessage *message)
{
  return (value)message;
}

/* TODO: finalizer */
CAMLprim value caml_opal_free_message(value msg)
{
  OpalMessage *message = Message_val(msg);

  caml_enter_blocking_section();
  FreeMessageFunction(message);
  caml_leave_blocking_section();

  return Val_unit;
}

CAMLprim value caml_opal_get_message(value h, value timeout)
{
  OpalHandle hOPAL = Handle_val(h);
  OpalMessage *message;

  caml_enter_blocking_section();
  message = GetMessageFunction(hOPAL, UINT_MAX); /* TODO: use timeout */
  caml_leave_blocking_section();

  assert(message);

  return val_message(message);
}

CAMLprim value caml_opal_get_message_type(value msg)
{
  CAMLparam1(msg);
  CAMLlocal1(ans);
  OpalMessage *message = Message_val(msg);

  switch (message->m_type)
  {
    case OpalIndIncomingCall:
      ans = caml_alloc(7, 0);
      Store_field(ans, 0, caml_copy_string(message->m_param.m_incomingCall.m_callToken));
      Store_field(ans, 1, caml_copy_string(message->m_param.m_incomingCall.m_localAddress));
      Store_field(ans, 2, caml_copy_string(message->m_param.m_incomingCall.m_remoteAddress));
      Store_field(ans, 3, caml_copy_string(message->m_param.m_incomingCall.m_remotePartyNumber));
      Store_field(ans, 4, caml_copy_string(message->m_param.m_incomingCall.m_remoteDisplayName));
      Store_field(ans, 5, caml_copy_string(message->m_param.m_incomingCall.m_calledAddress));
      Store_field(ans, 6, caml_copy_string(message->m_param.m_incomingCall.m_calledPartyNumber));
      CAMLreturn(ans);

    case OpalIndAlerting:
      break;

    default:
      break;
  }

  CAMLreturn(Val_int(0));
}

CAMLprim value caml_opal_answer_call(value h, value token)
{
  OpalHandle hOPAL = Handle_val(h);
  OpalMessage command;
  OpalMessage *response;
  char *t = malloc(caml_string_length(token));

  memcpy(t, String_val(token), caml_string_length(token));
  memset(&command, 0, sizeof(command));
  command.m_type = OpalCmdAnswerCall;
  command.m_param.m_callToken = t;
  assert(response = MySendCommand(hOPAL, &command));
  FreeMessageFunction(response);

  free(t);
  return Val_unit;
}
