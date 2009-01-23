#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

#include <opal.h>
#include <limits.h>
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

/* TODO: not a global value */
value write_cb;

int MyWriteMediaData(const char *token, const char *id, const char *format, void *userData, void *data, int size)
{
  value vdata;

  caml_leave_blocking_section();

  vdata = caml_alloc_string(size);
  memcpy(String_val(vdata), data, size);
  /* Also give the token and the id? */
  caml_callback2(write_cb, caml_copy_string(format), vdata);
  caml_enter_blocking_section();

  return size;
}

CAMLprim value caml_opal_set_general_parameters(value h, value auto_rx_media, value auto_tx_media, value write_callback)
{
  OpalHandle hOPAL = Handle_val(h);
  OpalMessage command;
  OpalMessage *response;

  memset(&command, 0, sizeof(command));
  command.m_type = OpalCmdSetGeneralParameters;
  command.m_param.m_general.m_autoRxMedia = String_val(auto_rx_media);
  command.m_param.m_general.m_autoTxMedia = String_val(auto_tx_media);
  command.m_param.m_general.m_mediaWriteData = MyWriteMediaData;
  command.m_param.m_general.m_mediaDataHeader = OpalMediaDataPayloadOnly; /* TODO: param */

  write_cb = write_callback;
  caml_register_global_root(&write_cb); /* TODO: unregister */

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
