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

# @flag hidden
def rewrite_metadata(l,~insert_missing=true,
                       ~update=true,~strip=false,
                       s)
  log(label="lang.deprecated",level=2,
      "WARNING: \"rewrite_metadata\"  is deprecated! Please use \"map_metadata\"")
  def map(m)
    def apply(x)
      label = fst(x)
      value = snd(x)
      (label,value % m)
    end
    list.map(apply,l)
  end
  map_metadata(map,insert_missing=insert_missing,
               update=update,strip=strip,s)
end
