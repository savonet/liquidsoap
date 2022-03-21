Liquidsoap supports dynamic creation and destruction of sources 
during the execution of a script. The following gives an example
of this.

First some outlines:

* This example is meant to create a new source and outputs. It is not easy currently to change a source being streamed
* The idea is to create a new output using a telnet/server command.
* In order for a Liquidsoap script to run without an active source at startup, it is necessary to include `settings.init.force_start.set(true)` at the start of the script. 


In this example, we will register a command that creates a playlist source using an uri passed
as argument and outputs it to a fixed icecast output.

With more work on parsing the argument passed to the telnet command,
you may write more evolved options, such as the possibility to change
the output parameters etc..

Please note that not all sources can be shutdown. Only outputs and sources that active can be shutdown.
You will be able to know that by the fact that only those two types of variables have a `shutdown()`
method.

New here's the code:

```liquidsoap
# Allow the script to start even without any output
settings.init.force_start.set(true)

# First, we create a list referencing the dynamic outputs shutdown methods:
dyn_outputs = ref([])

# This is our icecast output.
out = fun (mount, s) -> output.icecast(%mp3,
                              host="test",
                              password="hackme",
                              fallible=true,
                              mount=mount,
                              s)

count = ref(0)

# Now we write a function to create
# a playlist source and output it.
def create_playlist(uri) =
  # The playlist source
  s = playlist(uri)

  # The output
  mount = "dynamic-playlist-#{!count}"
  count := !count + 1
  o = out(mount, s)

  # We register the output shutdown method in the list of outputs
  # Storing the shutdown method instead of the whole output makes
  # it easier for the typer when using outputs with potentially
  # different methods.
  dyn_outputs := list.append( [(uri,o.shutdown)], !dyn_outputs)
  "Done!"
end

# And a function to destroy a dynamic output
def destroy_playlist(uri) = 
  # We need to find the source in the list,
  # remove it and destroy it. Currently, the language
  # lacks some nice operators for that so we do it
  # the functional way

  # This function is executed on every item in the list
  # of dynamic outputs
  def parse_list(ret, current_element) = 
    # ret is of the form: (matching_outputs, remaining_outputs)
    # We extract those two:
    matching_outputs = fst(ret)
    remaining_outputs = snd(ret)

    # current_element is of the form: ("uri", source) so 
    # we check the first element
    current_uri = fst(current_element)
    if current_uri == uri then
      # In this case, we add the source to the list of
      # matched sources
      (list.append( [snd(current_element)], 
                     matching_outputs),
       remaining_outputs)
    else
      # In this case, we put the element in the list of remaining
      # sources
      (matching_outputs,
       list.append([current_element], 
                    remaining_outputs))
    end
  end
    
  # Now we execute the function:
  result = list.fold(parse_list, ([], []), !dyn_outputs)
  matching_outputs = fst(result)
  remaining_outputs = snd(result)

  # We store the remaining sources in dyn_outputs
  dyn_outputs := remaining_outputs

  # If no source matched, we return an error
  if list.length(matching_outputs) == 0 then
    "Error: no matching sources!"
  else
    # We stop all matched outputs
    list.iter((fun (shutdown) -> shutdown()), matching_outputs)
    # And return
    "Done!"
  end
end


# Now we register the telnet commands:
server.register(namespace="dynamic_playlist",
                description="Start a new dynamic playlist.",
                usage="start <uri>",
                "start",
                create_playlist)
server.register(namespace="dynamic_playlist",
                description="Stop a dynamic playlist.",
                usage="stop <uri>",
                "stop",
                destroy_playlist)
```

If you execute this code (add a `output.dummy(blank())` if you have
no other output..), you have two new telnet commands:

* `dynamic_playlist.start <uri>`
* `dynamic_playlist.stop <uri>`

which you can use to create/destroy dynamically your sources.

With more tweaking, you should be able to adapt these ideas to your
precise needs.

If you want to plug those sources into an existing output, you may
want to use an `input.harbor` in the main output and change the
`output.icecast` in the dynamic source creation to send everything to
this `input.harbor`. You can use the `%wav` format in this case to avoid
compressing/decompressing the data..


