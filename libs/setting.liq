let setting = ()

# Create a getter from a setting.
# @param ~default Default value.
# @param name Name of the setting.
def setting.getter(~default, name) =
  def set(v)
    # Ensure that v has the same type as default
    ignore (default == v)
    set(name, v)
  end
  {get(default=default, name)}.{set=set}
end

# Create a getter from an integer setting.
# @param name Name of the setting.
def setting.getter.int(name)
  setting.getter(default=-1, name)
end

# Create a getter from a float setting.
# @param name Name of the setting.
def setting.getter.float(name)
  setting.getter(default=-1., name)
end

# Create a getter from a bool setting.
# @param name Name of the setting.
def setting.getter.bool(name)
  setting.getter(default=false, name)
end
