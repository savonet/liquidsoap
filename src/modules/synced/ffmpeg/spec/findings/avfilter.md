# avfilter — findings

Each finding in Defects, Asymmetries and API gaps ends with its verdict from an adversarial second read. Gaps are **read only** unless a verdict is given. Nothing was run.

## Defects

### Argument list is rendered in reverse order

`avfilter/avfilter.ml:219-237`. The accumulator prepends each rendered
argument and the result is joined without reversing, so
``[`Flag "a"; `Pair ("k", v)]`` becomes `k=v:a`. Order is irrelevant for
`key=value` pairs but positional (shorthand) values are order-sensitive in
FFmpeg's option-string parser, and a positional value after a named one is
rejected (see To verify). A rewrite that emits list order changes
behaviour for callers that compensated.

**confirmed (second read)** — `args_of_args` (`avfilter.ml:219-237`) prepends and `String.concat` never reverses; a `List.rev` or an append would have refuted it. FFmpeg side read in `ff_filter_opt_parse` (`libavfilter/avfilter.c`, same logic at n7.1.5, n8.1.3, n9.0.2): the first `key=value` sets `priv_class = NULL`, later tokens are parsed without `AV_OPT_FLAG_IMPLICIT_KEY`, and `av_opt_get_key_value` returns `AVERROR(EINVAL)` ("No option name near ...") for a token with no key. So ``[`Flag "a"; `Pair (k, v)]`` renders `k=v:a` and `attach` fails, while ``[`Pair (k, v); `Flag "a"]`` renders `a:k=v` and works; several `` `Flag `` values bind to the shorthand options in the reverse of list order.

### `get_array_separator` fails for array options that use the default separator

`avfilter/avfilter_stubs.c:613-619`. The separator is returned only when
`default_val.arr` is non-NULL and `arr->sep` is non-zero; otherwise the stub
falls through to `caml_failwith("Invalid filter!")`. FFmpeg treats a NULL
`arr` or a zero `sep` as the default separator `,` (To verify). So
``attach ~args:[`Pair (k, `Array _)]`` raises `Failure` for every array
option that does not declare a custom separator. The message is also the
"filter" one for what is a per-option condition.

**confirmed (second read)** — `opt_array_sep` in `libavutil/opt.c` is `(d && d->sep) ? d->sep : ','` at n7.1.5, n8.1.3 and n9.0.2, so the stub's fall-through to `Failure "Invalid filter!"` is wrong for both a NULL `arr` and a zero `sep`. Reach, from `git grep AV_OPT_TYPE_FLAG_ARRAY -- libavfilter`: at n7.1.5 only `aformat` has array options (separator `|`, works); at n8.1.3 and n9.0.2 every array option of `buffersink` and `abuffersink` (`pixel_formats`, `colorspaces`, `colorranges`, `sample_formats`, `samplerates`, `channel_layouts`, ...) declares no `arr`, so `` `Array `` arguments on the two sinks always raise.

### `.mli` says `Failure` "if the option does not exist"; a missing filter also raises, with a different message

`avfilter/avfilter.mli:61-63`, `avfilter/avfilter_stubs.c:601-602,610-611`.
Three messages, two of them identical for different causes.

**confirmed (second read)** — read the three `caml_failwith` sites (`avfilter_stubs.c:602,611,619`); no other path returns for a missing filter.

### `node_args` is never read

`avfilter/avfilter.ml:327-336`, `avfilter/avfilter.mli:121-125`. The public
`parse_node` record requires a `node_args` field that `parse` ignores.

**confirmed (second read)** — `get_ctx` destructures `{ node_name; node_pad; _ }` and the stub receives only `(string * filter_ctx * int)` triples; no other reader of `node_args` in the library.

### Pads that are neither audio nor video are classified as video

`avfilter/avfilter.ml:114-124`, `avfilter/avfilter_stubs.c:51-69`. The stub
produces `` `Data ``, `` `Subtitle ``, `` `Attachment ``, `` `Unknown ``; the
split sends everything that is not `` `Audio `` to the `video` list and
relabels it `` `Video ``. Such a pad can then be `link`ed to a video pad by
type.

**narrowed** — the code does what is described, but the case is latent: no filter in libavfilter at n7.1.5, n8.1.3 or n9.0.2 declares a pad whose type is not audio or video (`git grep '\.type *= *AVMEDIA_TYPE_[SD]' -- libavfilter` is empty at all three tags). It is a wrong fallback for a future or third-party filter, not a reachable wrong result today.

### Local channel layout is never uninitialised

`avfilter/avfilter_stubs.c:471-486`. `av_buffersink_get_ch_layout` fills a
stack `AVChannelLayout` with a copy; the avutil wrapper deep-copies it
again; the stack copy is never passed to `av_channel_layout_uninit`. A
custom-order layout owns a heap map, which leaks on every call. The error
path of the wrapper (raise) skips it too.

**confirmed (second read)** — `av_buffersink_get_ch_layout` (`libavfilter/buffersink.c`) does `av_channel_layout_copy` into a local and assigns it to `*out`, so the caller owns a copy; `value_of_channel_layout` (`avutil/avutil_stubs.c:418-436`) copies again into its own allocation and takes no ownership of its argument. The leak is real only for `AV_CHANNEL_ORDER_CUSTOM` layouts (the only order with a heap map).

### OCaml heap read after the runtime lock is released

`avfilter/avfilter_stubs.c:510-511` (`Filter_graph_val(_graph)`),
`:525-526` (`Frame_val(_frame)`), `:375` and `:274-275` (`Int_val` of
parameters). The accessors sit in the argument list of the FFmpeg call,
after `caml_release_runtime_system()`. The values are rooted, so the roots
are updated if the block moves, but the read itself races with a collector
running on another thread. The other stubs read into C locals before the
release.

**narrowed** — holds for `Filter_graph_val(_graph)` (`:511`) and `Frame_val(_frame)` (`:526`): both dereference a custom block after `caml_release_runtime_system()`, and both blocks are small enough to be allocated in the minor heap, so another thread's minor collection can move them during the read. It does not hold for `Int_val(_flags)` (`:275`) and `Int_val(_srcpad)`/`Int_val(_dstpad)` (`:375`): those read the C parameter itself, an immediate that no collection rewrites, and touch no heap.

### `parse` leaks on its out-of-memory paths and does not check label copies

`avfilter/avfilter_stubs.c:225-249,299-315`. `av_strdup` of the label is
unchecked (a NULL name is stored). When a list-element allocation fails,
the helper frees the list being built and raises; the just-duplicated
`name` and, when failing on outputs, the complete `inputs` list are leaked.
Labels are copied with `av_strdup` (stops at the first NUL) while the
description uses `av_strndup` with the OCaml length.

**narrowed** — the unchecked `av_strdup`, the leaked `name` and the leaked `inputs` list hold as read (`append_avfilter_in_out` frees only the list it was given, then raises). A NULL label is tolerated by FFmpeg (`extract_inout` skips entries with a NULL `name`), so it yields an unmatched label, not a crash. The last sentence is wrong: `av_strndup` (`libavutil/mem.c`) also stops at the first NUL (`memchr(s, 0, len)`), so labels and description are truncated alike.

### `.mli` documents `process_command` as acting on a "filter pad"

`avfilter/avfilter.mli:112-118`. It takes a filter and calls
`avfilter_process_command` on its context.

**confirmed (second read)** — the signature takes ``[ `Attached ] filter`` and the stub calls `avfilter_process_command` on its context.

### A failed `parse` frees every filter of the graph; all OCaml contexts dangle

**found in verification** (memory safety). `avfilter/avfilter_stubs.c:328-340`,
`avfilter/avfilter.ml:327-345`. On any error `avfilter_graph_parse_ptr` runs
`while (graph->nb_filters) avfilter_free(graph->filters[0]);` at its `end:`
label (`libavfilter/graphparser.c`, identical at n7.1.5, n8.1.3 and n9.0.2):
that frees the filters attached earlier with `attach`, not only those the
description created. The stub raises `Avutil.Error` and leaves everything
else in place: every `filter_ctx` held by attached pads, by
``[`Attached] filter`` values and by `config.audio_inputs` etc. now points to
freed memory, and `names` still lists them. A caller that catches the error
(a syntax error in a user-supplied description is enough) and goes on to
`link`, `process_command` or `launch` reaches freed contexts; `launch`
configures the now empty graph and returns input functions and output
handlers over them.

## Asymmetries

### Only three stubs keep the graph alive

`avfilter/avfilter.ml:349-364` passes the graph handle to `write_frame`,
`write_eof_frame`, `get_frame` "to make sure that \_config is not GCed". The
sink accessors (`avfilter.ml:65-92`), `link` (`:278-286`) and
`process_command` (`:294-305`) take a bare filter context. `'a context` is
public (`output.context`): a caller that keeps only the context and drops
the `output` record, the `t` and the `config` holds a dangling pointer once
the graph is finalised. `process_command` drops its only path to the graph
(the filter's pads) before entering a stub that releases the lock.

**confirmed (second read)** — checked what else could root the graph: attached pads carry `_config` and the `launch` closures capture `graph.c`, but `output.context`, the `filter_ctx` passed to the `link` stub and the one extracted by `get_context` are bare abstract blocks with no path to the graph. See also the failed-`parse` defect, which invalidates contexts while the graph is alive.

### `process_command` copies strings with `av_malloc`+`memcpy`, `attach` and `parse` with `av_strndup`

`avfilter/avfilter_stubs.c:260-271` versus `:185-192,317`. Same effect for
NUL-free strings.

**confirmed (second read)** — both forms copy up to the OCaml length and the callee stops at the first NUL.

### Graph custom block uses `mem=1, max=0`

`avfilter/avfilter_stubs.c:163`. The `av` container uses `0, 1`. With a
zero `max` the runtime's handling differs between OCaml versions (To
verify); either way it is unrelated to the graph's real size.

**confirmed (second read)** — the arguments are as cited (`avfilter_stubs.c:163`; `avutil` uses `0, 1` at `avutil_stubs.c:434`). The runtime's treatment of `max = 0` was not read and stays in To verify.

### `find` cannot return the four buffer/sink filters

`avfilter/avfilter.ml:160-169,187-188`. They are removed from `filters`, so
`find "abuffer"` raises `Not_found` while `abuffer` exists as a value. The
`.mli` calls `filters` the "Filter list".

**confirmed (second read)** — the fold's `match _name` puts the four names in separate accumulators and only the `_` branch conses onto `filters`.

## Gaps

### The hidden context field makes ``[`Attached] filter`` unsafe to rebuild

`avfilter/avfilter.ml:253-256,298-305`, `avfilter/avfilter_stubs.c:345-366`.
`filter` is a public, non-private record. `{ f with flags = [] }` on an
attached filter, or a literal record with empty pad lists annotated
``[`Attached] filter``, typechecks and has five fields; `process_command`
then takes the `io` field as a filter context and dereferences it. Nothing
states that attached filters must only come from `attach`.

### `f->description` may be NULL

`avfilter/avfilter_stubs.c:114`. `caml_copy_string(f->description)` with no
check. FFmpeg builds configured with small size compile descriptions out
(To verify); module initialisation would crash.

**confirmed (second read)** — `NULL_IF_CONFIG_SMALL(x)` expands to `NULL` under `CONFIG_SMALL` (`libavutil/internal.h:92`) and every filter description uses it, so on an `--enable-small` FFmpeg `caml_copy_string(NULL)` runs at module initialisation.

### No ordering between configuration and running

`avfilter/avfilter.ml:366-400`. `launch` may be called repeatedly and
`attach`/`link`/`parse` may follow it; `config` is the same type before and
after. A second `launch` reconfigures the C graph and returns fresh
closures over the same contexts.

### `parse` output is invisible to the bookkeeping

`avfilter/avfilter.ml:327-345`. Filters created by the description are not
in `names` (no `Exists` protection), have no handle (no `process_command`),
and sources/sinks written in the description never appear in `launch`'s
result.

### No escaping in argument strings

`avfilter/avfilter.ml:212-217`. `` `String `` values and array elements are
inserted verbatim; `:`, `=`, `'`, `\` and the array separator are not
escaped.

### Same-graph requirement of `link` is not expressed

`avfilter/avfilter.ml:285-286`. Pads carry their graph but `link` does not
compare them; FFmpeg's check is relied upon.

### No synchronisation on a graph

All graph stubs release the runtime lock (`avfilter_stubs.c:201-203,
327-329,374-376,510-512,525-527,560-562`) with no per-graph lock. Concurrent
push/pull on one graph from two threads reaches libavfilter concurrently.

### Unknown pad type raises nothing; unknown flag bits are dropped

`avfilter/avfilter_stubs.c:67-68`, `avfilter/avfilter.ml:140-150`.

## API gaps

### A filter graph cannot be released explicitly

`avfilter/avfilter.mli:91`. `init` has no counterpart; release is by
finaliser only, and the graph may hold threads and large frame queues.

**confirmed (second read)** — no stub other than the finaliser calls `avfilter_graph_free`.

### `'a context` outlives what guarantees its validity

`avfilter/avfilter.mli:41-59`. See "Only three stubs keep the graph alive".
The accessor signatures take only the context.

**confirmed (second read)** — same reading as "Only three stubs keep the graph alive".

### `'a filter` is a constructible record but ``[`Attached] filter`` has a hidden invariant

`avfilter/avfilter.mli:32-38,113-118`. See the first Gap. The type admits a
call (`process_command` on a hand-built or updated record) that is memory
unsafe.

**confirmed (second read)** — `filter` is exported with its five fields and no `private`; `ocaml_avfilter_get_content` returns `Field(_filter, Wosize - 1)` unconditionally, which on a five-field record is `io`, and `AvFilterContext_val` then reads the first word of that OCaml record as an `AVFilterContext *`.

### `parse_node.node_args` has no effect

`avfilter/avfilter.mli:121-125`.

**confirmed (second read)** — see the defect of the same content.

### Buffer sources and sinks are not reachable through `find` / `filters`

`avfilter/avfilter.mli:68-82`.

**confirmed (second read)** — see the asymmetry of the same content.

### No operation for the end-of-stream/flush state of a source after `` `Flush ``

`avfilter/avfilter.mli:40`. Once flushed, a source accepts nothing more
(FFmpeg behaviour, To verify) and there is no reset; the graph must be
rebuilt.

**confirmed (second read)** — `av_buffersrc_add_frame_flags` (`libavfilter/buffersrc.c`, n7.1.5 and n8.1.3) closes the source on a NULL frame, which sets `s->eof`, and returns `AVERROR_EOF` for every later frame; the binding exposes no way to clear it.

### `launch` after `launch`, and configuration after `launch`, are typed as valid

`avfilter/avfilter.mli:91-138`.

**confirmed (second read)** — `launch : config -> t` and `attach`/`link`/`parse` take the same `config`; what a second `avfilter_graph_config` does was not read.

## To verify

- FFmpeg's option-string parser rejects a positional value after a
  `key=value` pair (`av_opt_set_from_string`). Basis for the severity of the
  reversed-order finding.
- FFmpeg uses `,` as array separator when `default_val.arr` is NULL or its
  `sep` is 0 (`libavutil/opt.c`).
- `avfilter_graph_parse_ptr` on failure frees every filter in the graph,
  including those attached earlier; if so, all OCaml filter contexts and
  attached pads of that `config` dangle after a failed `parse`
  (`avfilter/avfilter_stubs.c:328-340`).
- `avfilter_graph_create_filter` frees the context itself when filter
  initialisation fails (the stub does not, `avfilter_stubs.c:210-211`).
- `AVFilter.description` is NULL in `--enable-small` builds.
- Whether any registered filter exposes a non-audio, non-video pad.
- Calling `avfilter_graph_config` twice on one graph.
- `av_buffersrc_write_frame(ctx, NULL)` marks EOF and later writes fail.
- `av_buffersink_set_frame_size` is effective when called after
  `avfilter_graph_config` (the converter calls it after `launch`,
  `avfilter/avfilter.ml:491-495`).
- `av_buffersink_get_channels` is still declared in the newest libavfilter
  the project targets.
- `caml_alloc_custom` with `max = 0` on the supported OCaml versions.

Settled in verification (FFmpeg read at n7.1.5, n8.1.3, n9.0.2):

- Positional after `key=value`: rejected with `AVERROR(EINVAL)`; filter
  arguments go through `ff_filter_opt_parse`, which has the same rule as
  `av_opt_set_from_string`.
- Default array separator: `,` when `default_val.arr` is NULL or `sep` is 0.
- Failed `avfilter_graph_parse_ptr`: frees every filter in the graph. See
  the defect added above.
- `avfilter_graph_create_filter` frees the context itself when
  `avfilter_init_str` fails and sets `*filt_ctx = NULL`; the stub has
  nothing to free.
- `AVFilter.description` is NULL in `--enable-small` builds.
- No registered filter exposes a non-audio, non-video pad.
- A NULL frame marks the source EOF and later writes return `AVERROR_EOF`.
- `av_buffersink_set_frame_size` after `avfilter_graph_config` is effective:
  it writes `min_samples`/`max_samples` of the sink's input link directly.
- `av_buffersink_get_channels` is declared in `buffersink.h` at all three
  tags.

Still open: `avfilter_graph_config` twice; `caml_alloc_custom` with
`max = 0`.
