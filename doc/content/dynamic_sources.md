Liquidsoap supports dynamic creation and destruction of sources 
during the execution of a script. The following gives an example
of this.

First some outlines:

* This example is meant to create a new source and outputs. It is not easy currently to change a source being streamed
* The idea is to create a new output using a telnet/server command.

In this example, we will register a command that creates a playlist source using an uri passed
as argument and outputs it to a fixed icecast output.

With more work on parsing the argument passed to the telnet command,
you may write more evolved options, such as the possibility to change
the output parameters etc..

Due to some limitations of the language, we have used some
intricate (but classic) functional programming tricks. They are
commented in order to help reading the code..

New here's the code:

```
# First, we create a list referencing the dynamic sources:
dyn_sources = ref []

# This is our icecast output.
# It is a partial application: the source needs to be given!
out = output.icecast(%mp3,
                     host="test",
                     password="hackme",
                     fallible=true)

# Now we write a function to create 
# a playlist source and output it.
def create_playlist(uri) =
  # The playlist source 
  s = playlist(uri)

  # The output
  output = out(s)

  # We register both source and output 
  # in the list of sources
  dyn_sources := 
      list.append( [(uri,s),(uri,output)],
                    !dyn_sources )
  "Done!"
end

# And a function to destroy a dynamic source
def destroy_playlist(uri) = 
  # We need to find the source in the list,
  # remove it and destroy it. Currently, the language
  # lacks some nice operators for that so we do it
  # the functional way

  # This function is executed on every item in the list
  # of dynamic sources
  def parse_list(ret, current_element) = 
    # ret is of the form: (matching_sources, remaining_sources)
    # We extract those two:
    matching_sources = fst(ret)
    remaining_sources = snd(ret)

    # current_element is of the form: ("uri", source) so 
    # we check the first element
    current_uri = fst(current_element)
    if current_uri == uri then
      # In this case, we add the source to the list of
      # matched sources
      (list.append( [snd(current_element)], 
                     matching_sources),
       remaining_sources)
    else
      # In this case, we put the element in the list of remaining
      # sources
      (matching_sources,
       list.append([current_element], 
                    remaining_sources))
    end
  end
    
  # Now we execute the function:
  result = list.fold(parse_list, ([], []), !dyn_sources)
  matching_sources = fst(result)
  remaining_sources = snd(result)

  # We store the remaining sources in dyn_sources
  dyn_sources := remaining_sources

  # If no source matched, we return an error
  if list.length(matching_sources) == 0 then
    "Error: no matching sources!"
  else
    # We stop all sources
    list.iter(source.shutdown, matching_sources)
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


