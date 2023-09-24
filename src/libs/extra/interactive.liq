# Information about all variables
variables        = ref([])
# Float variables
variables_float  = ref([])
# Int variables
variables_int    = ref([])
# Bool variables
variables_bool   = ref([])
# String variables
variables_string = ref([])
# Unit variables: those are not references but handler functions
variables_unit   = ref([])

let interactive        = ()
let interactive.float  = ()
let interactive.int    = ()
let interactive.bool   = ()
let interactive.string = ()
let interactive.unit   = ()

let interactive.error = error.register("interactive.error")

# @flag hidden
def interactive.list(_)
  l = variables()
  l = list.map(fun(xv) -> begin
    let (x,v) = xv
    "#{x} : #{v.type}"
  end, l)
  string.concat(separator="\n", l)
end
server.register(usage="list", description="List available interactive variables.", namespace="var", "list", interactive.list)

# Description of an interactive variable.
# @flag hidden
def interactive.description(name)
  list.assoc(name, variables()).description
end

# Type of an interactive variable.
# @flag hidden
def interactive.type(name)
  list.assoc(name, variables()).type
end

# @flag hidden
def interactive.float.ref(name)
  list.assoc(name, variables_float()).ref
end

# @flag hidden
def interactive.int.ref(name)
  list.assoc(name, variables_int()).ref
end

# @flag hidden
def interactive.bool.ref(name)
  list.assoc(name, variables_bool()).ref
end

# @flag hidden
def interactive.string.ref(name)
  list.assoc(name, variables_string()).ref
end

# @flag hidden
def interactive.unit.handler(name)
  list.assoc(name, variables_unit()).handler
end

# @flag hidden
def interactive.remove(name)
  variables := list.assoc.remove.all(name, variables())
end

# Remove an interactive variable.
# @param name Name of the variable.
# @category Interaction
# @flag hidden
def interactive.float.remove(name)
  interactive.remove(name)
  variables_float := list.assoc.remove.all(name, variables_float())
end

# Remove an interactive variable.
# @param name Name of the variable.
# @category Interaction
# @flag hidden
def interactive.int.remove(name)
  interactive.remove(name)
  variables_int := list.assoc.remove.all(name, variables_int())
end

# Remove an interactive variable.
# @param name Name of the variable.
# @category Interaction
# @flag hidden
def interactive.bool.remove(name)
  interactive.remove(name)
  variables_bool := list.assoc.remove.all(name, variables_bool())
end

# Remove an interactive variable.
# @param name Name of the variable.
# @category Interaction
# @flag hidden
def interactive.string.remove(name)
  interactive.remove(name)
  variables_string := list.assoc.remove.all(name, variables_string())
end

# Remove an interactive variable.
# @param name Name of the variable.
# @category Interaction
# @flag hidden
def interactive.unit.remove(name)
  interactive.remove(name)
  variables_unit := list.assoc.remove.all(name, variables_unit())
end

# Function called to ensure persistency of data.
let interactive.persistency = ref(fun()->())

# @flag hidden
def interactive.float.set(name, v)
  interactive.float.ref(name) := v
  p = interactive.persistency()
  p()
end

# @flag hidden
def interactive.int.set(name, v)
  interactive.int.ref(name) := v
  p = interactive.persistency()
  p()
end

# @flag hidden
def interactive.bool.set(name, v)
  interactive.bool.ref(name) := v
  p = interactive.persistency()
  p()
end

# @flag hidden
def interactive.string.set(name, v)
  interactive.string.ref(name) := v
  p = interactive.persistency()
  p()
end

# @flag hidden
def interactive.unit.set(name)
  f = interactive.unit.handler(name)
  f()
end

%ifdef osc.on_float
let stdlib_osc = osc
%endif

# Create an interactive variable.
# @flag hidden
# @param ~name Name of the variable.
# @param ~description Description of the variable.
# @param ~osc OSC address for the variable.
# @param ~type Type of the variable.
def interactive.create(~name, ~description="", ~osc="", ~type)
  if list.assoc.mem(name, variables()) then error.raise(interactive.error, "variable already registered") end
  variables := (name, { type=type, description=description }) :: variables()
  variables := list.sort(fun(n, n') -> if fst(n) < fst(n') then -1 else 1 end, variables())
%ifdef osc.on_float
  if osc != "" then
    if type == "float" then
      stdlib_osc.on_float(osc, fun(x) -> interactive.float.set(name, x))
    elsif type == "int" then
      stdlib_osc.on_int(osc, fun(x) -> interactive.int.set(name, x))
    elsif type == "bool" then
      stdlib_osc.on_bool(osc, fun(x) -> interactive.bool.set(name, x))
    elsif type == "string" then
      stdlib_osc.on_string(osc, fun(x) -> interactive.string.set(name, x))
    elsif type == "unit" then
      () # TODO
    else
      error.raise(error.not_found)
    end
  end
%else
  ignore(osc)
%endif
end

# @flag hidden
def interactive.get(name)
try
    t = interactive.type(name)
    if t == "float" then
      r = interactive.float.ref(name)
      string.float(decimal_places=3, r())
    elsif t == "int" then
      r = interactive.int.ref(name)
      string(r())
    elsif t == "bool" then
      r = interactive.bool.ref(name)
      string(r())
    elsif t == "string" then
      r = interactive.string.ref(name)
      r()
    elsif t == "unit" then
      "()"
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
    arg = r/=/.split(arg)
    name = string.trim(list.nth(arg, 0))
    value = string.trim(list.nth(arg, 1))
    t = interactive.type(name)
    if t == "float" then
      interactive.float.set(name, float_of_string(value))
    elsif t == "int" then
      interactive.int.set(name, int_of_string(value))
    elsif t == "bool" then
      interactive.bool.set(name, bool_of_string(value))
    elsif t == "string" then
      interactive.string.set(name, value)
    elsif t == "unit" then
      interactive.unit.set(name)
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
# @category Interaction
# @flag extra
# @param fname Name of the file.
def interactive.save(fname)
  data = json.stringify({
    float = list.map(fun (nv) -> begin let (name, v) = nv; (name, v.ref()) end, variables_float()),
    int = list.map(fun (nv) -> begin let (name, v) = nv; (name, v.ref()) end, variables_int()),
    bool = list.map(fun (nv) -> begin let (name, v) = nv; (name, v.ref()) end, variables_bool()),
    string = list.map(fun (nv) -> begin let (name, v) = nv; (name, v.ref()) end, variables_string())
  })

  file.write(data=data, fname)
end

# Load the value of interactive variables from a file.
# @category Interaction
# @flag extra
# @param fname Name of the file.
def interactive.load(fname)
  vars = file.contents(fname)
  let json.parse ( vars: {
    float : [(string * float)],
    int : [(string * int)],
    bool : [(string * bool)],
    string : [(string * string)]
  } ) = vars

  list.iter(fun (nv) -> try interactive.float.set (fst(nv), snd(nv)) catch _ do log.important(label="interactive.load", "Variable #{fst(nv)} not found.") end, vars.float )
  list.iter(fun (nv) -> try interactive.int.set   (fst(nv), snd(nv)) catch _ do log.important(label="interactive.load", "Variable #{fst(nv)} not found.") end, vars.int   )
  list.iter(fun (nv) -> try interactive.bool.set  (fst(nv), snd(nv)) catch _ do log.important(label="interactive.load", "Variable #{fst(nv)} not found.") end, vars.bool  )
  list.iter(fun (nv) -> try interactive.string.set(fst(nv), snd(nv)) catch _ do log.important(label="interactive.load", "Variable #{fst(nv)} not found.") end, vars.string)
end

# Make the value of interactive variables persistent: they are loaded from the
# given file and stored there whenever updated. This function should be called
# after all interactive variables have been defined (variables not declared yet
# will not be loaded).
# @category Interaction
# @flag extra
# @param fname Name of the file.
def interactive.persistent(fname)
  if file.exists(fname) then
    interactive.load(fname)
  else
    interactive.save(fname)
  end
  interactive.persistency := {interactive.save(fname)}
end

# Expose interactive variables through habor http server. Once this is called,
# with default parameters, you can browse <http://localhost:8000/interactive> to
# change the value of interactive variables using sliders.
# @category Interaction
# @flag extra
# @param ~port Port of the server.
# @param ~transport Http transport. Use `http.transport.ssl` or http.transport.secure_transport`, when available, to enable HTTPS output
# @param ~uri URI of the server.
def interactive.harbor(~transport=http.transport.unix, ~port=8000, ~uri="/interactive")
  def webpage(request, response)
    form_data = request.data
    data = ref("")
    def add(s) =
      data := data() ^ s ^ "\n"
    end
    title = "Interactive values"
    add("<!DOCTYPE html><html><head>")
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
    # TODO: we could send only the updated value instead of sending them all
    add("<script>
    function send() {
      let interactive = document.getElementsByClassName('interactive');
      let data = '';
      for(var i=0; i<interactive.length; i++){
        if (interactive[i].type == 'checkbox') {
          if (interactive[i].checked) {
            interactive[i].value = 'true'
          } else {
            interactive[i].value = 'false'
          }
        }
        if (interactive[i].type != 'button') {
          data = data.concat(interactive[i].name+'='+encodeURIComponent(interactive[i].value))+'&';
        }
      }
      console.log('Posting: ' + data);
      let xmlHttp = new XMLHttpRequest();
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
    function sendUnit(name) {
      const body = name + '=';
      console.log('Posting: ' + body);
      fetch('#{uri}', {method: 'POST', body: body});
    }
    </script>")
    add("</head><body>")
    add("<h1>#{title}</h1>")
    def add_var(nv)
      let (name, v) = nv
      description = interactive.description(name)
      description =
        if description == "" then name else "#{description} (#{name})" end

      add("<label for=#{name}>#{string.escape.html(description)}</label>")
      common =
        "id='#{name}' name='#{name}' class='interactive' onchange=\"send()\""

      if
        v.type == "float"
      then
        v = list.assoc(name, variables_float())
        value = http.string_of_float(v.ref())
        if v.min == 0.-infinity or v.max == infinity then
          add("<input type='number' #{common} step='#{v.step}' value='#{value}'>")
        else
          min = http.string_of_float(v.min)
          max = http.string_of_float(v.max)
          step = http.string_of_float(v.step)
          value = http.string_of_float(v.ref())
          unit = if v.unit == "" then "" else " "^v.unit end
          add("<div><input type='range' #{common} min='#{min}' max='#{max}' step='#{step}' value='#{value}' oninput='document.getElementById(\"#{name}-value\").innerHTML = this.value+\"#{unit}\"'><text id='#{name}-value' style='inline'>#{value}#{unit}</text></div>")
        end
      elsif v.type == "int" then
        v = list.assoc(name, variables_int())
        value = string(v.ref())
        add("<input type='number' #{common} step='1' value='#{value}'>")
      elsif v.type == "bool" then
        v = list.assoc(name, variables_bool())
        c = (v.ref()) ? "checked" : ""
        add("<input type='checkbox' #{common} value='true' #{c}>")
      elsif v.type == "string" then
        v = list.assoc(name, variables_string())
        add("<input type='text' #{common} value='#{string.escape.html(v.ref())}'>")
      elsif
        v.type == "unit"
      then
        add(
          "<button type='button' #{common} onclick=\"sendUnit('#{name}')\">#{name}</button>"
        )
      else
        ()
      end
    end
    add("<form>")
    list.iter(add_var, variables())
    add("</form>")
    add("</body>")
    response.html(data())
  end

  harbor.http.register(transport=transport, port=port, method="GET", uri, webpage)

  def setter(request, _)
    data = url.split_args(request.body())
    def update(nv)
      let (name, v) = nv
      try
        t = interactive.type(name)
        if t == "float" then
          interactive.float.set(name, float_of_string(v))
        elsif t == "int" then
          interactive.int.set(name, int_of_string(v))
        elsif t == "bool" then
          interactive.bool.set(name, bool_of_string(v))
        elsif t == "string" then
          interactive.string.set(name, v)
        elsif t == "unit" then
          interactive.unit.set(name)
        end
      catch e do
        log.important(label="interactive.harbor", "Could not update variable #{name} (#{e.kind}: #{e.message}).")
      end
    end
    list.iter(update, data)
  end

  harbor.http.register(transport=transport, port=port, method="POST", uri, setter)
  log.important(label="interactive.harbor", "Website should be ready at <http://localhost:#{port}#{uri}>.")
end

# Read a float from an interactive input.
# @category Interaction
# @flag extra
# @param ~min Minimal value.
# @param ~max Maximal value.
# @param ~step Typical variation of the value.
# @param ~description Description of the variable.
# @param ~unit Unit for the variable.
# @param ~osc OSC address.
# @param name Name of the variable.
# @param v Initial value.
def replaces interactive.float(~min=0.-infinity, ~max=infinity, ~step=0.1, ~description="", ~unit="", ~osc="", name, v)
  interactive.create(name=name, description=description, osc=osc, type="float")
  r = ref(v)
  variables_float := (name, {ref=r, unit=unit, min=min, max=max, step=step }) :: variables_float()
  r.{set = fun(x) -> interactive.float.set(name, x), remove = {interactive.float.remove(name)}}
end

# Read an integer from an interactive input.
# @category Interaction
# @flag extra
# @param ~description Description of the variable.
# @param ~osc OSC address.
# @param name Name of the variable.
# @param v Initial value.
def replaces interactive.int(~description="", ~osc="", name, v)
  interactive.create(name=name, description=description, osc=osc, type="int")
  r = ref(v)
  variables_int := (name, {ref=r }) :: variables_int()
  r.{set = fun(x) -> interactive.int.set(name, x), remove = {interactive.int.remove(name)}}
end

# Read a boolean from an interactive input.
# @category Interaction
# @flag extra
# @param ~description Description of the variable.
# @param ~osc OSC address.
# @param name Name of the variable.
# @param v Initial value.
def replaces interactive.bool(~description="", ~osc="", name, v)
  interactive.create(name=name, description=description, osc=osc, type="bool")
  r = ref(v)
  variables_bool := (name, { ref=r }) :: variables_bool()
  r.{set = fun(x) -> interactive.bool.set(name, x), remove = {interactive.bool.remove(name)}}
end

# Read a string from an interactive input.
# @category Interaction
# @flag extra
# @param ~description Description of the variable.
# @param ~osc OSC address.
# @param name Name of the variable.
# @param v Initial value.
def replaces interactive.string(~description="", ~osc="", name, v)
  interactive.create(name=name, description=description, osc=osc, type="string")
  r = ref(v)
  variables_string := (name, { ref=r }) :: variables_string()
  r.{set = fun(x) -> interactive.string.set(name, x), remove = {interactive.string.remove(name)}}
end

# Register a callback when a unit interactive input is set.
# @category Interaction
# @flag extra
# @param ~description Description of the variable.
# @param ~osc OSC address.
# @param name Name of the variable.
# @param f Function triggered when the value is set.
def replaces interactive.unit(~description="", ~osc="", name, f)
  interactive.create(name=name, description=description, osc=osc, type="unit")
  variables_unit := (name, { handler=f }) :: variables_unit()
  {set = f, remove = {interactive.float.remove(name)}}
end

# Create a multiband compressor whose parameters are interactive variables.
# @category Interaction
# @flag extra
# @param ~id Id of the source. Variable names are prefixed with this.
# @param ~bands Number of bands.
# @param s Source to compress.
def compress.multiband.interactive(~id=null(), ~bands=5, s)
  id = string.id.default(default="compress.multiband.interactive", id)
  prefix = id^"_"
  wet = interactive.float(prefix^"wet", min=0., max=1., 1.)
  min_freq = 100.
  max_freq = 15000.
  def band(i)
    frequency = exp((ln(max_freq) - ln(min_freq)) * float_of_int(i) / float_of_int(bands-1) + ln(min_freq))
    log.important(label=id, "Adding a band at #{frequency} Hz.")
    frequency = interactive.float("#{prefix}frequency#{i}", unit="Hz", min=0., max=20000., step=10., frequency)
    attack = interactive.float("#{prefix}attack#{i}", unit="ms", min=0., max=1000., step=10., 100.)
    release = interactive.float("#{prefix}release#{i}", unit="ms", min=0., max=1000., step=10., 200.)
    threshold = interactive.float("#{prefix}threshold#{i}", unit="dB", min=-20., max=0., step=0.1, -10.)
    ratio = interactive.float("#{prefix}ratio#{i}", min=1., max=10., step=0.1, 4.)
    gain = interactive.float("#{prefix}gain#{i}", unit="dB", min=0., max=30., step=0.1, 3.)
    {frequency=frequency, attack=attack, release=release, threshold=threshold, ratio=ratio, gain=gain}
  end
  l = list.init(bands, band)
  compress.multiband(wet=wet, s, l)
end
