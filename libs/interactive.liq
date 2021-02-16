variables        = ref([])
variables_float  = ref([])
variables_bool   = ref([])
variables_string = ref([])

let interactive = ()
let interactive.var = ()

# @flag hidden
def interactive.var.list(_)
  l = !variables
  l = list.map(fun(xv) -> begin
    let (x,v) = xv
    "#{x} : #{v.type}"
  end, l)
  string.concat(separator="\n", l)
end
server.register(usage="list", description="List available interactive variables.", namespace="var", "list", interactive.var.list)

# @flag hidden
def interactive.var.get(name)
  try
    v = list.assoc(name, !variables)
    if v.type == "float" then
      r = list.assoc(name, !variables_float)
      string_of(!r)
    elsif v.type == "bool" then
      r = list.assoc(name, !variables_bool)
      string_of(!r)
    elsif v.type == "string" then
      r = list.assoc(name, !variables_string)
      !r
    else
      error.raise(error.not_found)
    end
  catch _ do
    "Variable not found."
  end
end
server.register(namespace="var", description="Get the value of a variable.", "get", interactive.var.get)

# @flag hidden
def interactive.var.set(arg)
  try
    arg = string.split(separator=" = ", arg)
    name = list.nth(arg, 0)
    value = list.nth(arg, 1)
    v = list.assoc(name, !variables)
    if v.type == "float" then
      r = list.assoc(name, !variables_float)
      r := float_of_string(value)
    elsif v.type == "bool" then
      r = list.assoc(name, !variables_bool)
      r := bool_of_string(value)
    elsif v.type == "string" then
      r = list.assoc(name, !variables_string)
      r := value
    else
      error.raise(error.not_found)
    end
    "Variable #{name} set."
  catch _ do
    "Syntax error or variable not found."
  end
end
server.register(usage="set <name> = <value>", description="Set the value of a variable.", namespace="var", "set", interactive.var.set)

# @flag hidden
def add_variable(~name, ~type)
  variables := list.add((name, { type = type }), !variables)
end

# Read a float from an interactive input.
# @category Interaction
# @param name Name of the variable.
# @param v Initial value.
def interactive.float(name, v)
  add_variable(name=name, type="float")
  r = ref(v)
  variables_float := list.add((name, r), !variables_float)
  ref.getter(r)
end

# Read a boolean from an interactive input.
# @category Interaction
# @param name Name of the variable.
# @param v Initial value.
def interactive.bool(name, v)
  add_variable(name=name, type="bool")
  r = ref(v)
  variables_bool := list.add((name, r), !variables_bool)
  ref.getter(r)
end

# Read a string from an interactive input.
# @category Interaction
# @param name Name of the variable.
# @param v Initial value.
def interactive.string(name, v)
  add_variable(name=name, type="string")
  r = ref(v)
  variables_string := list.add((name, r), !variables_string)
  ref.getter(r)
end

# def interactive.float(telnet=true, min=0.-infinity, max=infinity, name, v)
  # r = ref(v)
  # interactive.float(name,v)
# end
