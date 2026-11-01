# icecast.server: an Icecast-compatible streaming server, specified in
# src/libs/extra/spec/icecast-server/. A script starts it once with
# `server = icecast.server(parameters)`, `parameters` being one record of
# optional fields (parameters.md). The returned value's methods are:
#
# - `on_source_connect`, `on_source_disconnect`, `on_listener_connect`,
#   `on_listener_disconnect`, `on_metadata` register event handlers;
# - `stats()` returns the statistics snapshot;
# - `reload(parameters)` replaces the whole record, validated first;
# - `reopen_logs()` reopens the log files;
# - `attach_source(~headers, ~format, mount, source)` and
#   `detach_source(mount)` feed a mount from a Liquidsoap source.
#
# Public helpers build parts of the record: `icecast.server.refusal`,
# `icecast.server.redirect`, `icecast.server.decision.all`,
# `icecast.server.decision.url_auth`, `icecast.server.decision.access_lists`,
# `icecast.server.decision.htpasswd`, `icecast.server.headers.cors` and
# `icecast.server.preroll_cache`. The parts below are included in dependency
# order; server.liq defines `icecast.server` itself.
%include "icecast_server/settings.liq"
%include "icecast_server/headers.liq"
%include "icecast_server/addresses.liq"
%include "icecast_server/refusals.liq"
%include "icecast_server/parameter_checks.liq"
%include "icecast_server/parameters.liq"
%include "icecast_server/mount_parameters.liq"
%include "icecast_server/prerolls.liq"
%include "icecast_server/decisions.liq"
%include "icecast_server/url_auth_options.liq"
%include "icecast_server/url_auth_reply.liq"
%include "icecast_server/url_auth_request.liq"
%include "icecast_server/url_auth.liq"
%include "icecast_server/access.liq"
%include "icecast_server/events.liq"
%include "icecast_server/stats.liq"
%include "icecast_server/mounts.liq"
%include "icecast_server/mount_chain.liq"
%include "icecast_server/listener_sessions.liq"
%include "icecast_server/listeners.liq"
%include "icecast_server/mount_outputs.liq"
%include "icecast_server/mount_dump.liq"
%include "icecast_server/feeders.liq"
%include "icecast_server/sources.liq"
%include "icecast_server/source_admission.liq"
%include "icecast_server/attached.liq"
%include "icecast_server/source_admin.liq"
%include "icecast_server/source_commands.liq"
%include "icecast_server/source_url.liq"
%include "icecast_server/mount_requests.liq"
%include "icecast_server/reload.liq"
%include "icecast_server/snapshot.liq"
%include "icecast_server/state.liq"
%include "icecast_server/server.liq"
