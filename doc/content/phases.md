Liquidsoap execution phases
===========================
There are various stages of running liquidsoap:

* **Parsing**: read scripts and scripting expressions, can fail with syntax errors.
* **Static analysis**: infer the type of all expressions, leaves some type unknown and may fail with type errors.
* **Instantiation**: when script is executed, sources get created. Remaining unknown [stream types](stream_contents.html) are forced according to `frame.*.channels` settings, [clocks](clock.html) are assigned (but unknown clocks may remain) and some sources are checked to be [infallible](source.htmls). Each of these steps may raise an error.
* **Collection**: Unknown clocks become the default clock so that all sources are assigned to one clock. Active sources newly attached to clocks are initialized for streaming, shutdown sources are detached from their clocks, and clocks are started or destroyed as needed. Streaming has started.

Usually, liquidsoap is ran by passing one or several scripts and expressions to execute. Those expressions set up some sources, and outputs typically don't change anymore. If those initially provided active sources fail to be initialized (invalid parameter, fail to connect, etc.) liquidsoap will terminate with an error.

It is however possible to **dynamically** create active sources,
through registered server commands, event handlers, etc.
They will be initialized and run as statically created ones.
In **interactive** mode (passing the `--interactive` option)
it is also possible to input expressions in a liquidsoap prompt,
and their execution can trigger the creation of new outputs.

Outputs can be deactivated using `source.shutdown()`:
they will stop streaming and will be destroyed.

The full liquidsoap instance
can be shutdown using the `shutdown()` command.
