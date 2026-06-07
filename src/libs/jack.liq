%ifdef jack.server.connect
# Connect JACK output ports to JACK input ports pairwise.
# If there is only one input port, all output ports are connected to it.
# If there is only one output port, all input ports are connected to it.
# Raises `error.invalid` if the port counts are mismatched otherwise.
# @flag hidden
def jack_connect(input_ports, output_ports) =
  n_in = list.length(input_ports)
  n_out = list.length(output_ports)
  if
    n_in == 1
  then
    let [in_port] = input_ports
    list.iter(
      fun (out_port) -> jack.server.connect(in_port, out_port),
      output_ports
    )
  elsif
    n_out == 1
  then
    let [out_port] = output_ports
    list.iter(
      fun (in_port) -> jack.server.connect(in_port, out_port),
      input_ports
    )
  elsif
    n_in == n_out
  then
    ignore(
      list.fold(
        fun (remaining, out_port) ->
          begin
            let [in_port, ...rest] = remaining
            jack.server.connect(in_port, out_port)
            rest
          end,
        input_ports,
        output_ports
      )
    )
  else
    error.raise(
      error.invalid,
      "jack.connect: port count mismatch (#{n_out} output port(s) vs #{n_in} \
       input port(s))"
    )
  end
end

# @docof input.jack
# @argsof input.jack
# @method connect Connect the ports of this JACK input to the ports of a JACK output.
def replaces input.jack(%argsof(input.jack)) =
  s = input.jack(%argsof(input.jack))

  def s.connect(o) =
    input_connected = ref(false)
    output_connected = ref(false)
    s.on_wake_up(
      synchronous=true,
      fun () ->
        begin
          input_connected := true
          if output_connected() then jack_connect(s.ports(), o.ports()) end
        end
    )
    (
      o.on_wake_up(
        synchronous=true,
        fun () ->
          begin
            output_connected := true
            if input_connected() then jack_connect(s.ports(), o.ports()) end
          end
      )
    :
      unit
    )
  end

  s
end

# @docof output.jack
# @argsof output.jack
# @method connect Connect the ports of this JACK output to the ports of a JACK input.
def replaces output.jack(%argsof(output.jack), s) =
  o = output.jack(%argsof(output.jack), s)

  def o.connect(s) =
    input_connected = ref(false)
    output_connected = ref(false)
    (
      s.on_wake_up(
        synchronous=true,
        fun () ->
          begin
            input_connected := true
            if output_connected() then jack_connect(s.ports(), o.ports()) end
          end
      )
    :
      unit
    )
    o.on_wake_up(
      synchronous=true,
      fun () ->
        begin
          output_connected := true
          if input_connected() then jack_connect(s.ports(), o.ports()) end
        end
    )
  end

  o
end

# Return the capture interface of the JACK server suitable to connect to any jack input operator.
# @param ~server JACK server to query. Uses default server if `null`.
def jack.server.capture(~server=null) =
  on_wake_up = fun (~synchronous:(_:bool), fn) -> (fn() : unit)

  # system:capture_* ports are JackPortIsOutput (they source audio into the graph)
  ports =
    fun () -> jack.server.get_output_ports(server=server, flags=["is_physical"])
  connect =
    fun (o) ->
      (o.on_wake_up(synchronous=true, {jack_connect(o.ports(), ports())}) :
        unit
      )
  {on_wake_up = on_wake_up, ports = ports, connect = connect}
end

# Return the playback interface of the JACK server suitable to connect to any jack output operator.
# @param ~server JACK server to query. Uses default server if `null`.
def jack.server.playback(~server=null) =
  on_wake_up = fun (~synchronous:(_:bool), fn) -> (fn() : unit)

  # system:playback_* ports are JackPortIsInput (they sink audio from the graph)
  ports =
    fun () -> jack.server.get_input_ports(server=server, flags=["is_physical"])
  connect =
    fun (o) ->
      (o.on_wake_up(synchronous=true, {jack_connect(ports(), o.ports())}) :
        unit
      )
  {on_wake_up = on_wake_up, ports = ports, connect = connect}
end
%endif
