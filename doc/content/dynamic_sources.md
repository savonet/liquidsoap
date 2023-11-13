Liquidsoap supports dynamic creation and destruction of sources
during the execution of a script. The following gives an example
of this.

First some outlines:

- This example is meant to create a new source and outputs. It is not easy currently to change a source being streamed
- The idea is to create a new output using a telnet/server command.
- In order for a Liquidsoap script to run without an active source at startup, it is necessary to include `settings.init.force_start := true` at the start of the script.

In this example, we will register a command that dynamically create a new output based on an encoded stream
and output it to an arbitrary url, as supported by the ffmpeg copy encoder. This script can be used to create
a dynamic restreaming platform.

Here's the code:

```{.liquidsoap include="content/liq/dynamic-source.liq"}

```

After executing this script, you should see two telnet commands:

- `restream.start <uri>`
- `restream.stop <uri>`

which you can use to create/destroy dynamically your sources.
