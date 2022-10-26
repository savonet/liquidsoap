Profiling scripts
=================

Sometimes, some functions of your script are taking up time and you would like
to optimize those. We are not speaking here about the encoding of streams, which
usually takes the vast majority of the spent computing power, but of functions
written directly in Liquidsoap. In order to understand those better, Liquidsoap
has a _profiler_ which records all the function calls. It can be enabled with

```liquidsoap
profiler.enable()
```

(or by passing the `--profile` commandline flag of Liquidsoap) and the
statistics can be obtained with

```liquidsoap
print(profiler.stats.string())
```

It will output something like

```
function              self              total             calls

+                     0.359139919281    0.359139919281    302000
list.add              0.324638843536    442.74707818      202000
if                    0.242718935013    442.951756954     102002
list.cons             0.230906486511    442.277146816     101000
```

where each lines consists of a function, the time spent in the functions, the
time spent in the function and the functions it has called and the number of
calls to the function.
