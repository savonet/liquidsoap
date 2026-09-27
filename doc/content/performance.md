# Performance and monitoring

This page helps you find out why a script is slow and keep an eye on a
running script. It covers the catchup message that Liquidsoap logs when it
falls behind, the profiler for functions written in Liquidsoap, and
Prometheus metrics for monitoring. For the big picture of how a script
produces data, see [the streaming loop](./script_lifecycle.md#the-streaming-loop).

## Understanding latency

This section explains what the following log message means and what you can
do about it:

```
Latency is too high: we must catchup 1.42 seconds!
```

### What is latency?

To generate a media stream, Liquidsoap runs a _streaming loop_. Each round of
the loop produces a small chunk of audio or video data, called a frame, and
sends it to your outputs. A frame lasts `settings.frame.duration` seconds,
0.02 seconds by default.

If Liquidsoap takes less than 0.02 seconds to produce a 0.02 seconds frame, it
is on time, or even a bit ahead, and it waits before producing the next frame.

If Liquidsoap takes more than 0.02 seconds, it falls behind. It then produces
the next frames without waiting, to catch up. When the delay goes over
`settings.clock.log_delay_threshold` (0.2 seconds by default), you see the
catchup message. The message is logged at most once every
`settings.clock.log_delay` seconds (1 second by default).

The catchup message means that the system is too slow and has to produce data
faster to get back on track. When the delay reaches
`settings.clock.max_latency` (60 seconds by default, self-sync sources can set
their own limit), Liquidsoap gives up catching up, logs
`Too much latency! Resetting active sources...` and resets its active sources.

Some sources and outputs control their own timing, others let Liquidsoap
pace the streaming loop with the CPU clock. [Clocks](./clocks.md#automatic-clock-mode)
explains which is which, how Liquidsoap switches between them, and how to fix
synchronization conflicts.

## Diagnosing latency issues

When you see a catchup message, go through the following questions.

### 1. Is latency CPU-controlled?

If no self-synchronized component is active, Liquidsoap uses the CPU clock to
manage time. Any delay in computation then makes the stream fall behind. See
[automatic clock mode](./clocks.md#automatic-clock-mode) for which sources
and outputs control their own timing.

### 2. Should a source be self-synchronized?

Some sources, such as `input.ffmpeg`, have to be set as self-sync manually,
with `self_sync=true`. Typically, `input.ffmpeg` should be self-sync when
decoding a `libsrt` or `rtmp` input.

### 3. Can the system keep up?

If timing is CPU-controlled, Liquidsoap has to generate frames fast enough to
stay on schedule. When it cannot, you see the catchup warning.

Common culprits:

- The CPU is not fast enough to decode or encode in real time.
- Disk access is slow, especially with network filesystems like NFS.
- Code inside the streaming loop blocks.

Before version 2.4.0, all callbacks in Liquidsoap were synchronous (blocking).
Since 2.4.0, source callbacks take a mandatory `synchronous` parameter: pass
`synchronous=false` and the callback runs outside the streaming loop. See
[source callbacks](./callbacks.md).

### 4. Are other processes slowing things down?

Other system tasks, like `cron` jobs or background processes, can temporarily
use most of the CPU or disk resources and cause latency.

### Summary

Latency issues usually come down to one of two things:

1. Liquidsoap manages time itself and falls behind. Your system might be too
   slow for the task at hand.
2. A source or output should manage time and is not set to (`self_sync`).

If you are still stuck:

- Enable detailed logs with `log.level := 4`.
- Simplify your setup and add pieces back one by one.
- If slow functions written in Liquidsoap are the suspect, measure them with
  the [profiler](#profiling-scripts).
- Track the latency of individual sources with
  [`prometheus.latency`](#prometheuslatency).

## Profiling scripts

Sometimes, some functions of your script are taking up time and you would like
to optimize those. We are not speaking here about the encoding of streams, which
usually takes the vast majority of the spent computing power, but of functions
written directly in Liquidsoap. In order to understand those better, Liquidsoap
has a _profiler_ which records all the function calls. It can be enabled with

```liquidsoap
profiler.enable()
```

(or by passing the `--profile` command-line flag of Liquidsoap) and the
statistics can be obtained with

```liquidsoap
print(profiler.stats.string())
```

It outputs something like

```
function              self              total             calls

+                     0.359139919281    0.359139919281    302000
list.add              0.324638843536    442.74707818      202000
if                    0.242718935013    442.951756954     102002
list.cons             0.230906486511    442.277146816     101000
```

where each line consists of a function, the time spent in the function, the
time spent in the function and the functions it has called, and the number of
calls to the function.

## Prometheus reporting

When compiled with optional support for [mirage/prometheus](https://github.com/mirage/prometheus),
`liquidsoap` can export [prometheus](https://prometheus.io/) metrics.

The basic settings to enable exports are:

```{.liquidsoap include="prometheus-settings.liq"}

```

Common metric types, namely `gauge`, `counter` and `summary` are provided via the script language, as well
as a specialized operator to track sources' latencies. A fully-featured implementation can be found at
[mbugeia/srt2hls](https://github.com/mbugeia/srt2hls)

### Basic operators

The 3 basic operators are:

- `prometheus.counter`
- `prometheus.gauge`
- `prometheus.summary`

They share a similar type and API, which is as follows:

```liquidsoap
(help : string,
 ?namespace : string,
 ?subsystem : string,
 labels : [string],
 string) ->
   (label_values : [string]) ->
     (float) -> unit
```

This type can be a little confusing. Here's how it works:

1. First, one has to create a metric factory of a given type. For instance:

```{.liquidsoap include="prometheus-callback.liq" from="A0" to="A1"}

```

2. Then, the metric factory can be used to instantiate specific metrics by passing the label's values:

```{.liquidsoap include="prometheus-callback.liq" from="B0" to="B1"}

```

The returned function is a setter for this metric, i.e.

- For `gauge` metrics, it sets the gauge value
- For `counter` metrics, it increases the counter value
- For `summary` metrics, it registers an observation

Finally, the programmer can now use that callback to set the metric as desired. For instance here:

```{.liquidsoap include="prometheus-callback.liq" from="C0"}

```

### `prometheus.latency`

The `prometheus.latency` operator provides prometheus metrics describing the internal latency of a given
source. Like the basic operators, you first create it with label names, then apply it to label values and
a source:

```liquidsoap
s = (...)
latency_metric = prometheus.latency(labels=["source"])
latency_metric(label_values=["radio"], s)
```

The metrics are computed over a sliding window, set by the `window` parameter (5 seconds by default).
Metric names start with the `prefix` parameter (`liquidsoap_` by default). Exported metrics are:

```
# Input metrics:
liquidsoap_input_latency_ratio{...} <value>
liquidsoap_input_peak_latency_ratio{...} <value>
liquidsoap_input_max_latency_ratio{...} <value>

# Output metrics:
liquidsoap_output_latency_ratio{...} <value>
liquidsoap_output_peak_latency_ratio{...} <value>
liquidsoap_output_max_latency_ratio{...} <value>

# Overall metrics:
liquidsoap_overall_latency_ratio{...} <value>
liquidsoap_overall_peak_latency_ratio{...} <value>
liquidsoap_overall_max_latency_ratio{...} <value>

# Last time the source produced data:
liquidsoap_time_of_last_data_timestamp{...} <value>
```

The 3 different groups of values are:

- **input**: metrics related to the time it takes to generate audio data
- **output**: metrics related to the time it takes to output (encode and send) audio data
- **overall**: the sum of all previous two groups

Each group of metrics is divided into 3 subsets:

- `latency`: mean latency value over the sliding window
- `peak_latency`: peak latency value over the sliding window
- `max_latency`: max latency value since start

Latencies are reported relative to a frame's duration: a value of `1` means that it took as long to
process a frame as the frame lasts. Thus, in a situation where liquidsoap does not observe latency
catch-ups, the overall mean latency `liquidsoap_overall_latency_ratio` should stay below `1`.

These metrics can be used to report and track the source of latencies and catch-ups while streaming.
Typically, if a source starts taking too much time to generate its audio data, this should be reflected in the
`input` latencies. Likewise for encoding and network output.

Keep in mind, however, that enabling these metrics can have a CPU cost. It is rather small with a couple of sources
but can increase with the number of sources being tracked. The user of these metrics is advised to keep track of
CPU usage while ramping up on using them.

### OCaml specific metrics

The prometheus binding used by `liquidsoap` also exports default OCaml-related metrics. They are as follows:

```
ocaml_gc_allocated_bytes <value>
ocaml_gc_compactions <value>
ocaml_gc_heap_words <value>
ocaml_gc_major_collections <value>
ocaml_gc_major_words <value>
ocaml_gc_minor_collections <value>
ocaml_gc_top_heap_words <value>
process_cpu_seconds_total <value>
```

These metrics can be useful when debugging issues with `liquidsoap`, in particular to track if an observed increase in
memory usage is related to OCaml memory allocation or not. More often than not, if the increase is not related to OCaml,
it can be safely assumed that the issue might come from an external library used by `liquidsoap`.
