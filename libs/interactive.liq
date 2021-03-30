# Information about all variables
variables        = ref([])
# Float variables
variables_float  = ref([])
# Bool variables
variables_bool   = ref([])
# String variables
variables_string = ref([])

let interactive        = ()
let interactive.float  = ()
let interactive.bool   = ()
let interactive.string = ()

let interactive.error = error.register("interactive.error")

# @flag hidden
def interactive.list(_)
  l = !variables
  l = list.map(fun(xv) -> begin
    let (x,v) = xv
    "#{x} : #{v.type}"
  end, l)
  string.concat(separator="\n", l)
end
server.register(usage="list", description="List available interactive variables.", namespace="var", "list", interactive.list)

# @flag hidden
def interactive.description(name)
  list.assoc(name, !variables).description
end

# @flag hidden
def interactive.type(name)
  list.assoc(name, !variables).type
end

# @flag hidden
def interactive.float.ref(name)
  list.assoc(name, !variables_float).ref
end

# @flag hidden
def interactive.bool.ref(name)
  list.assoc(name, !variables_bool).ref
end

# @flag hidden
def interactive.string.ref(name)
  list.assoc(name, !variables_string).ref
end

let interactive.persistency = ref(fun()->())

# @flag hidden
def interactive.float.set(name, v)
  interactive.float.ref(name) := v
  p = !interactive.persistency
  p()
end

# @flag hidden
def interactive.bool.set(name, v)
  interactive.bool.ref(name) := v
  p = !interactive.persistency
  p()
end

# @flag hidden
def interactive.string.set(name, v)
  interactive.string.ref(name) := v
  p = !interactive.persistency
  p()
end

# Create an interactive variable.
# @flag hidden
def interactive.create(~name, ~description="", ~type)
  if list.mem_assoc(name, !variables) then error.raise(interactive.error, "variable already registered") end
  variables := list.add((name, { type=type, description=description }), !variables)
  variables := list.sort(fun(n, n') -> if fst(n) < fst(n') then -1 else 1 end, !variables)
end

# @flag hidden
def interactive.get(name)
try
    t = interactive.type(name)
    if t == "float" then
      r = interactive.float.ref(name)
      string_of(!r)
    elsif t == "bool" then
      r = interactive.bool.ref(name)
      string_of(!r)
    elsif t == "string" then
      r = interactive.string.ref(name)
      !r
    else
      error.raise(error.not_found)
    end
  catch _ do
    "Variable not found."
  end
end
server.register(namespace="var", description="Get the value of a variable.", "get", interactive.get)

# @flag hidden
def interactive.set(arg)
  try
    arg = string.split(separator=" = ", arg)
    name = list.nth(arg, 0)
    value = list.nth(arg, 1)
    t = interactive.type(name)
    if t == "float" then
      interactive.float.set(name, float_of_string(value))
    elsif t == "bool" then
      interactive.bool.set(name, bool_of_string(value))
    elsif t == "string" then
      interactive.string.set(name, value)
    else
      error.raise(error.not_found)
    end
    "Variable #{name} set."
  catch _ do
    "Syntax error or variable not found."
  end
end
server.register(usage="set <name> = <value>", description="Set the value of a variable.", namespace="var", "set", interactive.set)

# Save the value of all interactive variables in a file.
# @category Interactive
# @param fname Name of the file.
def interactive.save(fname)
  int = [] # Be ready when we add support for those
  float = list.map(fun (nv) -> begin let (name, v) = nv; (name, !v.ref) end, !variables_float)
  bool = list.map(fun (nv) -> begin let (name, v) = nv; (name, !v.ref) end, !variables_bool)
  string = list.map(fun (nv) -> begin let (name, v) = nv; (name, !v.ref) end, !variables_string)
  vars = (int, float, bool, string)
  file.write(data=json_of(vars), fname)
end

# Load the value of interactive variables from a file.
# @category Interactive
# @param fname Name of the file.
def interactive.load(fname)
  vars = file.contents(fname)
  let (int, float, bool, string) = of_json(default=([("",0)],[("",0.)],[("",false)],[("","")]), vars)
  ignore(int)
  list.iter(fun (nv) -> try interactive.float.set (fst(nv), snd(nv)) catch _ do log.important(label="interactive.load", "Variable #{fst(nv)} not found.") end, float )
  list.iter(fun (nv) -> try interactive.bool.set  (fst(nv), snd(nv)) catch _ do log.important(label="interactive.load", "Variable #{fst(nv)} not found.") end, bool  )
  list.iter(fun (nv) -> try interactive.string.set(fst(nv), snd(nv)) catch _ do log.important(label="interactive.load", "Variable #{fst(nv)} not found.") end, string)
end

# Make the value of interactive variables persistent: they are loaded from the
# given file and stored there whenever updated. This function should be called
# after all interactive variables have been defined (variables not declared yet
# will not be loaded).
# @category Interactive
# @param fname Name of the file.
def interactive.persistent(fname)
  if file.exists(fname) then interactive.load(fname) end
  interactive.persistency := {interactive.save(fname)}
end

# Expose interactive variables through habor http server.
# @category Interaction
# @flag extra
# @param ~port Port of the server.
# @param ~uri URI of the server.
def interactive.harbor(~port=8080, ~uri="/interactive")
  def webpage(~protocol, ~data, ~headers, uri)
    form_data = data
    data = ref("")
    def add(s) =
      data := !data ^ s ^ "\n"
    end
    title = "Interactive values"
    add("<html><head>")
    add("<meta charset='utf-8'/>")
    add("<title>#{title}</title>")
    add("<style>
    body {
      font-family: sans-serif;
    }
    h1 {
      text-align: center;
    }
    form {
      border-radius: 20px;
      display: block;
      background-color: whitesmoke;
      width: max-content;
      margin: 0 auto;
      padding: 2ex;
      display:grid;
      grid-template-columns: max-content max-content;
      grid-gap: 5px;
    }
    label {
      text-align: right;
    }
    input {
      width: 300px;
    }
    </style>")
    # TODO: we could send only the udpated value instead of sending them all
    add("<script>
    function send() {
      var interactive = document.getElementsByClassName('interactive');
      var data = '';
      for(var i=0; i<interactive.length; i++){
        if (interactive[i].type == 'checkbox') {
          if (interactive[i].checked) {
            interactive[i].value = 'true'
          } else {
            interactive[i].value = 'false'
          }
        }
        data = data.concat(interactive[i].name+'='+interactive[i].value)+'&';
      }
      console.log(data);
      var xmlHttp = new XMLHttpRequest();
      xmlHttp.open('POST', '#{uri}');
      xmlHttp.onreadystatechange = function () {
        if(xmlHttp.readyState === XMLHttpRequest.DONE) {
          var status = xmlHttp.status;
          if (status === 0 || (status >= 200 && status < 400)) {
            //console.log(xmlHttp.responseText);
          } else {
            console.log('Failed to send values.')
          }
        }
      }
      xmlHttp.send(data);
    }
    </script>")
    add("</head><body>")
    add("<h1>#{title}</h1>")
    def add_var(nv)
      let (name, v) = nv
      description = interactive.description(name)
      description = if description == "" then name else "#{description} (#{name})" end
      # add("<div>")
      add("<label for=#{name}>#{description}</label>")
      common = "id='#{name}' name='#{name}' class='interactive' onchange=\"send()\""
      if v.type == "float" then
        v = list.assoc(name, !variables_float)
        value = http.string_of_float(!v.ref)
        if v.min == 0.-infinity or v.max == infinity then
          add("<input type='number' #{common} step='#{v.step}' value='#{value}'>")
        else
          min = http.string_of_float(v.min)
          max = http.string_of_float(v.max)
          step = http.string_of_float(v.step)
          value = http.string_of_float(!v.ref)
          unit = if v.unit == "" then "" else " "^v.unit end
          add("<div><input type='range' #{common} min='#{min}' max='#{max}' step='#{step}' value='#{value}' oninput='document.getElementById(\"#{name}-value\").innerHTML = this.value'><text id='#{name}-value' style='inline'>#{value}#{unit}</text></div>")
        end
      elsif v.type == "bool" then
        v = list.assoc(name, !variables_bool)
        c = (!v.ref) ? "checked" : ""
        add("<input type='checkbox' #{common} value='true' #{c}>")
      elsif v.type == "string" then
        v = list.assoc(name, !variables_string)
        add("<input type='text' #{common} value='#{!v.ref}'>")
      else
        ()
      end
      # add("</div>")
    end
    add("<form>")
    list.iter(add_var, !variables)
    # add("<input type='submit' value='Send'>")
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
          interactive.float.set(name, float_of_string(v))
        elsif t == "bool" then
          interactive.bool.set(name, bool_of_string(v))
        elsif t == "string" then
          interactive.string.set(name, v)
        end
      catch _ do
        log.important(label="interactive.harbor", "Could not update variable #{name}.")
      end
    end
    list.iter(update, data)
    http.response()
  end

  harbor.http.register(port=port, method="POST", uri, setter)
  log.important(label="interactive.harbor", "Website should be ready at <http://localhost:#{port}#{uri}>.")
end

# Read a float from an interactive input.
# @category Interaction
# @param ~min Minimal value.
# @param ~max Maximal value.
# @param ~step Typical variation of the value.
# @param ~description Description of the variable.
# @param ~unit Unit for the variable.
# @param name Name of the variable.
# @param v Initial value.
def replaces interactive.float(~min=0.-infinity, ~max=infinity, ~step=0.1, ~description="", ~unit="", name, v)
  interactive.create(name=name, description=description, type="float")
  r = ref(v)
  variables_float := list.add((name, {ref=r, unit=unit, min=min, max=max, step=step }), !variables_float)
  ref.getter(r).{set = interactive.float.set(name)}
end

# Read a boolean from an interactive input.
# @category Interaction
# @param ~description Description of the variable.
# @param name Name of the variable.
# @param v Initial value.
def replaces interactive.bool(~description="", name, v)
  interactive.create(name=name, description=description, type="bool")
  r = ref(v)
  variables_bool := list.add((name, { ref=r }), !variables_bool)
  ref.getter(r).{set = interactive.bool.set(name)}
end

# Read a string from an interactive input.
# @category Interaction
# @param ~description Description of the variable.
# @param name Name of the variable.
# @param v Initial value.
def replaces interactive.string(~description="", name, v)
  interactive.create(name=name, description=description, type="string")
  r = ref(v)
  variables_string := list.add((name, { ref=r }), !variables_string)
  ref.getter(r).{set = interactive.bool.set(name)}
end

# Create a multiband compressor whose parameters are interactive variables.
# @category Interaction
# @param ~id Id of the source. Variable names are prefixed with this.
# @param ~bands Number of bands.
# @param s Source to compress.
def compress.multiband.interactive(~id="compress", ~bands=5, s)
  prefix = id^"_"
  wet = interactive.float(prefix^"wet", min=0., max=1., 1.)
  def band(i)
    frequency = 100. + 10. * exp(ln(25000.) * float_of_int(i) / float_of_int(bands))
    frequency = interactive.float("#{prefix}frequency#{i}", unit="Hz", min=0., max=20000., step=10., frequency)
    attack = interactive.float("#{prefix}attack#{i}", unit="ms", min=0., max=1000., 100.)
    release = interactive.float("#{prefix}release#{i}", unit="ms", min=0., max=1000., 200.)
    threshold = interactive.float("#{prefix}threshold#{i}", unit="dB", min=-20., max=0., -10.)
    ratio = interactive.float("#{prefix}ratio#{i}", min=1., max=10., 4.)
    gain = interactive.float("#{prefix}gain#{i}", unit="dB", min=0., max=30., 3.)
    {frequency=frequency, attack=attack, release=release, threshold=threshold, ratio=ratio, gain=gain}
  end
  l = list.init(bands, band)
  compress.multiband(wet=wet, s, l)
end
