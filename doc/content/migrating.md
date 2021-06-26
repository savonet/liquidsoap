Migrating to a new Liquidsoap version
=====================================

In this page, we list the most common catches when migrating to a new version of
Liquidsoap.

From 1.4.x to 2.0.0
-------------------

### Http input and operators

In order to provide as much compatibility with the different HTTP procotols and implementation, we have decided
to delegate HTTP support to external libraries which have large scale support and implementation. This means that,
if you have installed `liquidsoap` using `opam`:

* You need to install the `ocurl` package to enable all HTTP request operators, `http.get`, `http.post`, `http.put`, `http.delete` and `http.head`
* You need to install the `ffmpeg` package (version `1.0.0` or above) to enable `input.http`
* You do not need to install the `ssl` package anymore to enable their `https` counter-part. These operators have been deprecated.

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

