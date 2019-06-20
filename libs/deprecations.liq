# Deprecated APIs
# @flag hidden
def deprecated() =
  def on_change_harbor_bind_addr(v) =
    log.severe(label="lang.deprecated",
        "WARNING: \"harbor.bind_addr\" is deprecated! Please use \"harbor.bind_addrs\"")
    set("harbor.bind_addrs",[v])
  end

  register(name="[DEPRECATED] Harbor bind_addr",
           descr="IP addresses on which the harbor should listen.",
           on_change=on_change_harbor_bind_addr,"harbor.bind_addr","0.0.0.0")    
end

deprecated()

# @flag hidden
def input.external(~id="",~buffer=2.0,~channels=2,~max=10.0,
                   ~restart=true,~restart_on_error=false,
                   ~samplerate=44100,cmd) =
  log.severe(label="lang.deprecated",
    "WARNING: \"input.external\" is deprecated! Please use \"input.external.wav\"")
  input.external.wav(id=id,buffer=buffer,channels=channels,
                     max=max,restart=restart,restart_on_error=restart_on_error,
                     samplerate=samplerate,cmd)
end
