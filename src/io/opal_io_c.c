#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#include <opal.h>
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
  assert(hOPAL = InitialiseFunction(&version, OPAL_PREFIX_H323 " " OPAL_PREFIX_SIP " " OPAL_PREFIX_IAX2 " " OPAL_PREFIX_LOCAL " TraceLevel=4"));

  return (value)hOPAL;
}

CAMLprim value caml_opal_shutdown(value h)
{
  ShutDownFunction(Handle_val(h));

  return Val_unit;
}

/* TODO: raise exceptions */
static OpalMessage *MySendCommand(OpalHandle hOPAL, OpalMessage *command, const char *errorMessage)
{
  OpalMessage *response;
  if ((response = SendMessageFunction(hOPAL, command)) == NULL)
    return NULL;
  if (response->m_type != OpalIndCommandError)
    return response;

  if (response->m_param.m_commandError == NULL || *response->m_param.m_commandError == '\0')
    printf("%s.\n", errorMessage);
  else
    printf("%s: %s\n", errorMessage, response->m_param.m_commandError);

  FreeMessageFunction(response);

  return NULL;
}

CAMLprim value caml_opal_set_protocol_parameters(value h, value username, value displayname, value interface)
{
  /* TODO: product */
  /* TODO: blocking section */
  OpalHandle hOPAL = Handle_val(h);
  OpalMessage   command;
  OpalMessage * response;

  memset(&command, 0, sizeof(command));
  command.m_type = OpalCmdSetProtocolParameters;

  command.m_param.m_protocol.m_userName = String_val(username);
  command.m_param.m_protocol.m_displayName = String_val(displayname);
  command.m_param.m_protocol.m_interfaceAddresses = String_val(interface);

  assert(response = MySendCommand(hOPAL, &command, "Could not set protocol options"));

  FreeMessageFunction(response);

  return Val_unit;
}

CAMLprim value caml_opal_listen(value h, value cb)
{
  OpalHandle hOPAL = Handle_val(h);
  /* HandleMessages(timeout, UINT_MAX); */

  return Val_unit;
}
