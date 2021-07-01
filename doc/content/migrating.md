Migrating to a new Liquidsoap version
=====================================

In this page, we list the most common catches when migrating to a new version of
Liquidsoap.

From 1.4.x to 2.0.0
-------------------

### `audio_to_stereo`

`audio_to_stereo` should not be required in most typicaly situations anymore. `liquidsoap` can handle channels conversions transparently now! 

### Http input and operators

In order to provide as much compatibility with the different HTTP procotols and implementation, we have decided
to delegate HTTP support to external libraries which have large scale support and implementation. This means that,
if you have installed `liquidsoap` using `opam`:

* You need to install the `ocurl` package to enable all HTTP request operators, `http.get`, `http.post`, `http.put`, `http.delete` and `http.head`
* You need to install the `ffmpeg` package (version `1.0.0` or above) to enable `input.http`
* You do not need to install the `ssl` package anymore to enable their `https` counter-part. These operators have been deprecated.

### Crossfade

The parameters for `cross` transitions was changed to take advantage of the new module system. Instead of passing multiple arguments
related to the ending and starting track, those are regrouped into a single record. So, if you had a transition like this:

```liquidsoap
def transition(
  ending_dB_level, starting_dB_level,
  ending_metadata, starting_metadata,
  ending_source,   starting_source) =
...
end
```

You would now do:

```liquidsoap
def transition(ending, starting) =
  # Now you can use:
  #  - ending.db_level, ending.metadata, ending.source
  #  - starting.db_level, starting.metadata, starting.source
...
end
```

### Deprecated operators

Some operators have been deprecated. For most of them, we provide a backward-compatible support 
but it is good practice to update your script. You should see logs in your script when running
deprecated operatords. Here's a list of the most important ones:

* `playlist.safe` is replaced by: `playlist(mksafe(..))`
* `playlist.once` is replaced by: `playlist`, setting `reload_mode` argument to `"never"` and `loop` to `false`
* `rewrite_metadata` should be rewritten using `map_metadata`
* `fade.inital` and `fade.final` are not needed anymore
* `get_process_output` is replaced by: `process.read`
* `get_process_lines` is replaced by: `process.read.lines`
* `test_process` is replaced by: `process.test`
* `system` is replaced by: `process.run`
* `add_timeout` is replaced by: `thread.run.recurrent`
* `on_blank` is replaced by: `blank.detect`
* `skip_blank` is replaced by: `blank.skip`
* `eat_blank` is replaced by: `blank.eat`
* `strip_blank` is replaced by: `blank.strip`
* `which` is replaced by: `file.which`
* `register_flow`: flow is no longer maintained
* `empty` is replaced by: `source.fail`
* `file.unlink` is replaced by: `file.remove`

### Windows build

The windows binary is statically built and, for this reason, we cannot enable both the `%ffmpeg` encoder and any encoder that
uses the same underlying libraries, for instance `libmp3lame` for `mp3` encoding. The technical reason is that both libraries
import the same C symbols, which makes compilation fail.

The `%ffmpeg` encoder provides all the functionalities of the internal encoders that conflict with along with more format
we do not support otherwise. For this reason, it was decided to enable the `%ffmpeg` encoder and disable all other encoders.

This means that, if you were previously using a different encoder than `%ffmpeg`, you will need to adapt your script to
use it. For instance, for mp3 encoding with variable bitrate:

```liquidsoap
%ffmpeg(format="mp3", %audio(codec="libmp3lame", q=7))
```
