# Observability (Part A)

What a clock tells about itself: a status record, log events, and two text
reports rendered from the record.

Binding throughout: which facts are present, their values, their order and
their nesting. Incidental: wording, glyphs, column widths, wrapping. The
layouts shown are recommendations.

## 1. Status record

Every clock gives, at any moment and from any thread, one record. A report is
a pure function of records: it never reads a clock.

| Field       | Value                                                                                                      |
| ----------- | ---------------------------------------------------------------------------------------------------------- |
| name        | [clock.md §2](clock.md#2-names)                                                                            |
| state       | `stopped`, `started`, `stopping`, and the stop reason of a stopped clock, with the error if it failed      |
| sync mode   | [clock.md §3](clock.md#3-sync-modes)                                                                       |
| controller  | Passive clocks: the controlling clock or external entity                                                   |
| animator    | Non-passive started clocks: `task` or `thread`, and why (`blocks: <sync source>`, `unsynced`, `tasks off`) |
| sync source | The tracked one, its pacing, and whether the clock follows it                                              |
| ticks       | No value when stopped                                                                                      |
| stream time | No value when stopped                                                                                      |
| lateness    | Seconds behind its time source; negative when ahead. No value when the clock does not measure it           |
| sources     | Pending, outputs, active, passive: each a list of source entries                                           |
| sub-clocks  | The records of its registered sub-clocks                                                                   |
| statistics  | Below                                                                                                      |

A **source entry** is an id, a type and the ids of the source's activations.

**Statistics**, over the life of the streaming state, each also over a recent
window:

| Figure                | Meaning                                                                     |
| --------------------- | --------------------------------------------------------------------------- |
| tick duration         | Mean and maximum real time of a tick, waits excluded                        |
| slowest source        | The animated source that took the longest in the slowest tick               |
| time producing        | Total real time inside ticks, waits excluded                                |
| time waiting in ticks | Total time in waits ([clock.md §9](clock.md#9-waiting-inside-a-tick))       |
| time resting          | Total time in rests, and their count                                        |
| time released         | Total time between a release and the clock being ready again, and the count |
| time without a worker | Total time between being ready and being given a worker                     |
| resets                | Count of latency resets                                                     |
| switches              | Count of sync source switches and of animator changes                       |

Keeping them MUST cost a constant per tick, plus a constant per animated
source.

These figures answer "why is this clock late": it produces slower than real
time (time producing against stream time), it waits on something (time
waiting in ticks), or it is kept from a worker (time released, time without a
worker).

## 2. Log events

Each event MUST be logged with the facts listed. A rate limit, where given,
is per clock.

| Event                     | Facts                                                                                                                                          | Level     |
| ------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------- | --------- |
| start                     | name; top-level or passive; controller; sync mode; sources as `id (type)`; animator and why                                                    | important |
| animator change           | from, to, why                                                                                                                                  | important |
| sync source switch        | from, to, pacing, latency and maximum latency in effect                                                                                        | important |
| stop                      | stop reason; ticks and stream time reached                                                                                                     | important |
| failure                   | the error, its backtrace, the failing source if any                                                                                            | critical  |
| source failure            | the source, the error, its backtrace, whether a handler took it                                                                                | severe    |
| latency warning           | lateness; since the last warning: stream produced, time producing, time waiting in ticks, time released, time without a worker; slowest source | severe    |
| latency reset             | lateness; the same breakdown; ticks before and after                                                                                           | severe    |
| long tick                 | a tick longer than the time box: duration, slowest source. At most one per latency log period                                                  | important |
| rest, release, wait       | deadline or condition, time actually spent                                                                                                     | debug     |
| leak warning              | total sources activated; the status report of the clock                                                                                        | severe    |
| id kept at merge          | both ids                                                                                                                                       | info      |
| sync source ended         | the sync source                                                                                                                                | important |
| still running at shutdown | each clock: name, what it is doing (in a tick since when, resting, waiting), its slowest source                                                | critical  |
| unknown time source       | the name asked for and the one used                                                                                                            | severe    |

Reading a clock's status never logs.

## 3. Status report

Input: status records. One block per top-level clock, in creation order,
separated by an empty line; sub-clocks nested under their parent.

```
main [started, auto, thread: blocks pulseaudio]
  sync source: pulseaudio (self-paced, followed)
  ticks: 2400  time: 96.00s  lateness: -
  tick: mean 0.4ms max 3.1ms (slowest: output.pulseaudio)
  held: producing 1.0s  waiting 0.0s  resting 0.0s  released 0.0s  no worker 0.0s
  outputs: output.pulseaudio [output.pulseaudio]
  active: -
  passive: mic [output.pulseaudio], amplify [output.pulseaudio]
  └─ cross.child [started, passive, controlled by cross]
       ticks: 2500  time: 100.00s  lateness: -
       outputs: cross.out [cross]
       passive: playlist [cross.out]

archive [stopped: failed (Connection refused), auto]
  pending: output.file
```

- Header: name, then state, sync mode, and for a started non-passive clock
  its animator and why; for a passive clock its controller. A stopped clock
  shows its stop reason.
- A stopped clock shows its pending sources and nothing else: no ticks, no
  time.
- "No value" is shown by one mark, the same everywhere.
- Source lists are in the order `pending`, `outputs`, `active`, `passive`;
  `pending` only when not empty. Items are sorted by id.
- A long list MAY be wrapped, only between two items.

The same records SHOULD be available in a structured form, for programs.

## 4. Source graph

Shows which source wakes which. Input: for one clock, a flat list of source
entries.

```
Clock main:
  output.icecast [output]
  └─ shared_encoder [passive]
     └─ audio [passive]
  output.file [output]
  └─ shared_encoder [passive] (see above)
  Woken from outside this clock:
  cross [external]
  └─ cross.out [output]
  Not woken:
  spare [passive]
```

- If `S` lists `A` among its activations, `S` is printed under `A`. A source's
  own id among its activations is ignored.
- Roots, in order: each output that no other source of the list activates, in
  input order.
- An activation id that matches no source in the list is an **external
  activator**. Sources activated only from outside are listed under their
  activator in a second section, activators sorted by id.
- Sources with no activation at all are listed in a third section.
- A source already printed is printed again with a mark and nothing beneath
  it.
- An empty section is omitted.

The application's report prints one such graph per clock of the application,
then one per sub-clock with its parent named in the header, separated by an
empty line.
