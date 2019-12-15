# Deprecated APIs.

# Mark a function as deprecated.
# @flag hidden
# @category Liquidsoap
# @param old Old function name.
# @param new New function name.
def deprecated(old, new)
  new = if new == "" then "" else " Please use \"#{new}\" instead." end
  log.severe(label="lang.deprecated", "WARNING: \"#{old}\" is deprecated and will be removed in future version.#{new}")
end

# @flag hidden
def on_change_harbor_bind_addr(v) =
  deprecated("harbor.bind_addr", "harbor.bind_addrs")
  set("harbor.bind_addrs",[v])
end

register(name="[DEPRECATED] Harbor bind_addr",
         descr="IP addresses on which the harbor should listen.",
         on_change=on_change_harbor_bind_addr,"harbor.bind_addr","0.0.0.0")

# Deprecated: this function has been replaced by input.external.rawaudio.
# @flag hidden
def input.external(~id="",~buffer=2.0,~channels=2,~max=10.0,
                   ~restart=true,~restart_on_error=false,
                   ~samplerate=44100,cmd) =
  deprecated("input.external", "input.external.rawaudio")
  input.external.rawaudio(id=id,buffer=buffer,channels=channels,
                          max=max,restart=restart,restart_on_error=restart_on_error,
                          samplerate=samplerate,cmd)
end

# Deprecated: this function has been replaced by quote.
# @flag hidden
def quote(s) =
  deprecated("quote", "string.quote")
  string.quote(s)
end

# Deprecated: use mksafe and playlist instead.
# @flag hidden
def playlist.safe(~id="", ~mime_type="", ~mode="randomize", ~on_track={()}, ~prefix="", ~reload=0, ~reload_mode="seconds", uri)
  deprecated("playlist.safe", "")
  mksafe(playlist(id=id, mime_type=mime_type, mode=mode, prefix=prefix, reload=reload, reload_mode=reload_mode, uri))
end

# Deprecated: this function has been replaced by thread.run.recurrent.
# @flag hidden
def add_timeout(~fast=true, delay, f)
  deprecated("add_timeout", "thread.run.recurrent")
  thread.run.recurrent(fast=fast, delay=delay, f)
end

# Deprecated: this function has been replaced by thread.when.
# @flag hidden
def exec_at(~freq=1., ~pred, f)
  deprecated("exec_at", "thread.when")
  thread.when(every=freq, pred, f)
end

# Deprecated: this function has been replaced by thread.mutexify.
# @flag hidden
def mutexify(f)
  deprecated("mutexify", "thread.mutexify")
  thread.mutexify(f)
end

# Deprecated: this function has been replaced by file.which.
# @flag hidden
def which(f)
  deprecated("which", "file.which")
  file.which(f)
end

# Deprecated: this function has been replaced by string.base64.decode.
# @flag hidden
def base64.decode(s)
  deprecated("base64.decode", "string.base64.decode")
  string.base64.decode(s)
end

# Deprecated: this function has been replaced by string.base64.encode.
# @flag hidden
def base64.encode(s)
  deprecated("base64.encode", "string.base64.encode")
  string.base64.encode(s)
end
