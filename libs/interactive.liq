variables        = ref([])
variables_float  = ref([])
variables_bool   = ref([])
variables_string = ref([])

let interactive = ()
let interactive.ref = ()
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
def interactive.type(name)
  list.assoc(name, !variables).type
end

# @flag hidden
def interactive.ref.float(name)
  list.assoc(name, !variables_float).ref
end

# @flag hidden
def interactive.ref.bool(name)
  list.assoc(name, !variables_bool).ref
end

# @flag hidden
def interactive.ref.string(name)
  list.assoc(name, !variables_string).ref
end

# @flag hidden
def interactive.var.get(name)
try
    t = interactive.type(name)
    if t == "float" then
      r = interactive.ref.float(name)
      string_of(!r)
    elsif t == "bool" then
      r = interactive.ref.bool(name)
      string_of(!r)
    elsif t == "string" then
      r = interactive.ref.string(name)
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
    t = interactive.type(name)
    if t == "float" then
      r = interactive.ref.float(name)
      r := float_of_string(value)
    elsif t == "bool" then
      r = interactive.ref.bool(name)
      r := bool_of_string(value)
    elsif t == "string" then
      r = interactive.ref.string(name)
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
  def webpage(~protocol, ~data, ~headers, uri)
    form_data = data
    data = ref("")
    def add(s) =
      data := !data ^ s ^ "\n"
    end
    title = "Interactive values"
    add("<html><head>")
    add("<title>#{title}</title>")
    add("<script src='https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js'></script>")
    # TODO: we could send only the udpated value instead of sending them all
    ajax = list.map(fun (x) -> begin
      x = fst(x)
      "#{x}: $('##{x}').val()"
    end, !variables)
    ajax = string.concat(separator=", ", ajax)
    add("<script> \
    function send() {
      var data = {#{ajax}};
      console.log(data);
      $.ajax({ uri: '#{uri}', type: 'POST', data: data })
    }
    </script>")
    add("</head><body>")
    add("<h1>#{title}</h1>")
    def add_var(nv)
      let (name, v) = nv
      add("<div>")
      add("<label for=#{name}>#{name}</label>")
      common = "id='#{name}' name='#{name}' onchange=\"send()\""
      if v.type == "float" then
        v = list.assoc(name, !variables_float)
        value = string_of(!v.ref) ^ "0"
        if v.min == 0.-infinity or v.max == infinity then
          add("<input type='number' #{common} step='#{v.step}' value='#{value}'>")
        else
          add("<input type='range' #{common} min='#{v.min}' max='#{v.max}' step='#{v.step}' value='#{!v.ref}'>")
        end
      elsif v.type == "bool" then
        v = list.assoc(name, !variables_bool)
        c = (!v.ref) ? "checked" : ""
        add("<input type='checkbox' #{common} #{c}>")
      elsif v.type == "string" then
        v = list.assoc(name, !variables_string)
        add("<input type='text' #{common} value='#{!v.ref}'>")
      else
        ()
      end
      add("</div>")
    end
    add("<form>")
    list.iter(add_var, !variables)
    add("<input type='submit' value='Send'>")
    add("</form>")
    add("</body>")
    http.response(data=!data)
  end

  harbor.http.register(port=port, method="GET", uri, webpage)

  def setter(~protocol, ~data, ~headers, uri)
    data = url.split_args(data)
    def update(nv)
      let (name, v) = nv
      try
        t = interactive.type(name)
        if t == "float" then
          r = interactive.ref.float(name)
          r := float_of_string(v)
        elsif t == "bool" then
          r = interactive.ref.bool(name)
          # TODO: not working for now
        elsif t == "string" then
          r = interactive.ref.string(name)
          r := v
        end
      catch _ do
        log.important("Could not update variable #{name}")
      end
    end
    list.iter(update, data)
    http.response()
  end

  harbor.http.register(port=port, method="POST", uri, setter)
end

# Read a float from an interactive input.
# @category Interaction
# @param name Name of the variable.
# @param v Initial value.
def interactive.float(~min=0.-infinity, ~max=infinity, ~step=0.1, name, v)
  add_variable(name=name, type="float")
  r = ref(v)
  variables_float := list.add((name, { ref=r , min=min, max=max, step=step }), !variables_float)
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
