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
      r = list.assoc(name, !variables_float).ref
      string_of(!r)
    elsif v.type == "bool" then
      r = list.assoc(name, !variables_bool).ref
      string_of(!r)
    elsif v.type == "string" then
      r = list.assoc(name, !variables_string).ref
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
      r = list.assoc(name, !variables_float).ref
      r := float_of_string(value)
    elsif v.type == "bool" then
      r = list.assoc(name, !variables_bool).ref
      r := bool_of_string(value)
    elsif v.type == "string" then
      r = list.assoc(name, !variables_string).ref
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
  variables := list.add((name, { type=type }), !variables)
  variables := list.sort(fun(n, n') -> if fst(n) < fst(n') then -1 else 1 end, !variables)
end

# Expose interactive variables through habor http server.
# @category Interaction
def interactive.harbor(~port=8080, uri="interactive")
  def handler(~protocol, ~data, ~headers, uri)
    data = ref("")
    def add(s) =
      data := !data ^ s ^ "\n"
    end
    title = "Interactive values"
    add("<html><head><title>#{title}</title></head><body>")
    add("<h1>#{title}</h1>")
    def add_var(nv)
      let (name, v) = nv
      add("<div>")
      add("<label for=#{name}>#{name}</label>")
      if v.type == "float" then
        v = list.assoc(name, !variables_float)
        add("<input type='range' id='#{name}' name='#{name}' min='#{v.min}' max='#{v.max}' value='#{!v.ref}'>")
      elsif v.type == "bool" then
        v = list.assoc(name, !variables_bool)
        c = (!v.ref) ? "checked" : ""
        add("<input type='checkbox' id='#{name}' name='#{name}' #{c}>")
      elsif v.type == "string" then
        v = list.assoc(name, !variables_string)
        add("<input type='text' id='#{name}' name='#{name}' value='#{!v.ref}'>")
      else
        ()
      end
      add("</div>")
    end
    list.iter(add_var, !variables)
    add("</body>")
    http.response(data=!data)
  end
  harbor.http.register(port=port, method="GET", uri, handler)
end

# Read a float from an interactive input.
# @category Interaction
# @param name Name of the variable.
# @param v Initial value.
def interactive.float(~min=0.-infinity, ~max=infinity, name, v)
  add_variable(name=name, type="float")
  r = ref(v)
  variables_float := list.add((name, { ref=r , min=min, max=max }), !variables_float)
  ref.getter(r)
end

# Read a boolean from an interactive input.
# @category Interaction
# @param name Name of the variable.
# @param v Initial value.
def interactive.bool(name, v)
  add_variable(name=name, type="bool")
  r = ref(v)
  variables_bool := list.add((name, { ref=r }), !variables_bool)
  ref.getter(r)
end

# Read a string from an interactive input.
# @category Interaction
# @param name Name of the variable.
# @param v Initial value.
def interactive.string(name, v)
  add_variable(name=name, type="string")
  r = ref(v)
  variables_string := list.add((name, { ref=r }), !variables_string)
  ref.getter(r)
end
