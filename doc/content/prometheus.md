Prometheus reporting
====================

When compiled with optional support for [mirage/prometheus](https://github.com/mirage/prometheus), 
`liquidsoap` can export [prometheus](https://prometheus.io/) metrics. 

The basic settings to enable exports are:
```liquidsoap
# Prometheus settings
set("prometheus.server",true)
set("prometheus.server.port",9090)
```

Common metrics, namely `gauge`, `counter` and `summary` are provided via the script language, as well
as a specialized operator to track source's latencies. A fully-featured implementation can be found at
[mbugeia/srt2hls](https://github.com/mbugeia/srt2hls)

Basic operators
---------------

The 3 basic operators are:
* `prometheus.counter`
* `prometheus.gauge`
* `prometheus.summary`

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
```liquidsoap
is_playing_metric = prometheus.gauge(labels=["source"],"liquidsoap_is_playing")
```
2. Then, the metric factory can be used to instantiate speific metrics by passing the label's values:
```liquidsoap
playlist = playlist(id="playlist", ...)
set_playlist_is_playing = is_playing_metric(label_values=["radio"])
```
The returned function is a setter for this metric, i.e.
* For `gauge` metrics, it sets the gauge value
* For `counter` metrics, it increases the counter value
* For `summary` metrics, it registers an observation

Finally, the programmer can now use that callback to set the metric as desired. For instance here:
```liquidsoap
def check_if_ready(set_is_ready, source) =
  def callback() =
    if source.is_ready(source) then
      set_is_ready(1.)
    else
      set_is_ready(0.)
    end
    0.1
  end
  callback
end
thread.run.recurrent(delay=0.,check_if_ready(set_playlist_is_playing, playlist))
```

`prometheus.latency`
--------------------

The `prometheus.latency` operator provides prometheus metrics describing the internal latency of a given
source. It is fairly easy to use:
```liquidsoap
s = (...)
prometheus.latency(s)
```
The metrics are computed over a sliding window that can be defined as a parameter of the operator. Exported metrics are:
```
# Input metrics:
liquidsoap_input_latency{...} <value>
liquidsoap_input_max_latency{...} <value>
liquidsoap_input_peak_latency{...} <value>

# Output metrics:
liquidsoap_outputput_latency{...} <value>
liquidsoap_output_max_latency{...} <value>
liquidsoap_output_peak_latency{...} <value>

# Overall metrics:
liquidsoap_overall_latency{...} <value>
liquidsoap_overall_max_latency{...} <value>
liquidsoap_overall_peak_latency{...} <value>
```

The 3 different groups of values are:
* **input**: metrics related to the time it takes to generate audio data
* **output**: metrics related to the time it takes to output (encode and send) audio data
* **overall**: the sum of all previous two groups

Each group of metrics is divided into 3 subsets:
* Mean latency value over the sliding window
* Max latency value over the sliding window
* Peak latency since start

Latencies are reported over a frame's duration, which is typically around `0.04` seconds. Thus, in a situation
where liquidsoap does not observe latency catch-ups, the overall mean latency `liquidsoap_overall_latency` should
always be near that value.

These metrics can be used to report and track the source of latencies and catch-ups while streaming.
Typically, if a source starts taking too much time to generate its audio data, this should be reflects in the 
`input` latencies. Likewise for encoding and network output.

Keep in mind, however, that enabling these metrics can have a CPU cost. It is rather small with a couple of sources
but can increase with the number of sources being tracked. The user of these metrics is advised to keep track of
CPU usage while ramping up on using them.

OCaml specific metrics
----------------------

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

These metrics can be useful when debugging issues with `liquidsoap`, in particular to track is an observed increase in
memory usage is related to OCaml memory allocation or not. More than often, if the increase is not related to OCaml,
it can be safely assumed that the issue might come from an external library used by `liquisoap`.
