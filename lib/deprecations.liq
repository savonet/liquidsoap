# Deprecated APIs
# @flag hidden
def deprecated() =
  def on_change_harbor_bind_addr(v) =
    log(label="lang.deprecated",level=2,
        "WARNING: \"harbor.bind_addr\" is deprecated! Please use \"harbor.bind_addrs\"")
    set("harbor.bind_addrs",[v])
  end

  register(name="[DEPRECATED] Harbor bind_addr",
           descr="IP addresses on which the harbor should listen.",
           on_change=on_change_harbor_bind_addr,"harbor.bind_addr","0.0.0.0")    
end

deprecated()
