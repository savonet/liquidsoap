# Namespace, error kinds and process-wide settings of icecast.server
# (parameters.md §7, §10). Every other part attaches its helpers to the
# `icecast.server` namespace declared here, so the entry point MUST define the
# server with `def replaces icecast.server(parameters)`, which keeps them; a
# plain `def` drops them.
let icecast = ()
let icecast.server = ()

let error.icecast = ()
let error.icecast.server = ()
let error.icecast.server.invalid_parameters =
  error.register("error.icecast.server.invalid_parameters")

let error.icecast.server.mount_taken =
  error.register("error.icecast.server.mount_taken")

let error.icecast.server.reserved_path =
  error.register("error.icecast.server.reserved_path")

let error.icecast.server.format_mismatch =
  error.register("error.icecast.server.format_mismatch")

# @flag hidden
let settings.icecast.server =
  settings.make.void(
    "Icecast server settings"
  )

# @flag hidden
def icecast.server.no_tls_transport(_, _) =
  log.critical(
    label="icecast.server",
    "TLS is enabled but no TLS transport is available. Please install TLS or SSL \
     support and rebuild liquidsoap."
  )
  shutdown()
  http.transport.unix.{reload = fun () -> ()}
end

# Factory of the TLS transport that `icecast.server.config.load` builds. A pair
# of `(name, factory)` where `factory` takes getters of a certificate path and
# of an optional private key path and returns an HTTP transport with a `reload`
# method.
# @flag extra
# @category Internet
let settings.icecast.server.tls_transport =
  settings.make(
    description="Factory of the TLS transport that icecast.server.config.load \
     builds: a pair of (name, factory).",
    ("none", icecast.server.no_tls_transport)
  )

%ifdef http.transport.tls
settings.icecast.server.tls_transport :=
  (
    "tls",
    fun (certificate, key) ->
      http.transport.tls(certificate=certificate, key=key)
  )
%endif

%ifdef http.transport.ssl
settings.icecast.server.tls_transport :=
  (
    "ssl",
    fun (certificate, key) ->
      http.transport.ssl(certificate=certificate, key=key)
  )
%endif

# Muxer options of a container format when `muxer_options` is unset.
# @flag extra
# @category Internet
let settings.icecast.server.default_muxer_options =
  settings.make(
    description="Muxer options of a container format when the muxer_options \
     parameter of icecast.server is unset.",
    [("matroska", [("live", "1")]), ("webm", [("live", "1")])]
  )

# Bytes of the process-wide preroll store, read at each storage.
# @flag extra
# @category Internet
let settings.icecast.server.preroll_cache_size =
  settings.make(
    description="Bytes of prerolls kept in memory by all \
     icecast.server.preroll_cache views, the least recently used dropped first.",
    268435456
  )
