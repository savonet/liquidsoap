# Principles (Part A)

What holds in every part of Liquidsoap. Each principle has one owner, the
document that states it in full; the others link there. A component
document that restates one of them, or departs from one, says so and gives
its reason.

## Streaming

1. **Keep streaming, unless the error is major.** A fault is contained to
   the smallest thing it concerns and becomes a signal the script can act
   on. Only a broken promise stops a stream. Owner:
   [faults.md](faults.md#0-the-rule).
2. **Infallibility is a promise.** A source found infallible at start never
   leaves a frame short. A broken promise is a failure, raised loudly, never
   a glitch. Owner: [faults.md §3](faults.md#3-source-faults-reach-the-script).
3. **No source invents content.** No silence, black or repeated frame stands
   in for data a source cannot produce; filler is the script's choice.
   Owner: [faults.md §3](faults.md#3-source-faults-reach-the-script).
4. **A streaming cycle is static.** What a source answers does not change
   until the cycle is over. Ready means delivering now: nothing waits inside
   a cycle, except a blocking sync source whose wait is its pacing. Owner:
   [source-contract.md §1](../core/source/spec/source-contract.md#1-streaming-cycle).
5. **No commitment before the consumer takes data.** A decision is made when
   a source is probed and acted on when it is read. Owner:
   [source-contract.md §2](../core/source/spec/source-contract.md#2-probes).
6. **Explicit signals.** Nothing is deduced from being probed, read or
   ticked. The party that decides tells the parties it affects; every change
   of state has a named cause. Owner:
   [source-contract.md §9](../core/source/spec/source-contract.md#9-no-inference).
7. **A skip is a request.** It is carried out once the running cycle is
   over. Owner:
   [source-contract.md §6](../core/source/spec/source-contract.md#6-abort).
8. **A track mark sits on the logical start of content.** Filler carries
   none. Owner:
   [source-contract.md §3](../core/source/spec/source-contract.md#3-tracks).
9. **Nothing held is lost.** Every stage drains what it holds before it
   ends. Owner: [draining.md](../core/source/spec/draining.md).
10. **No wait is unbounded.** Owner: [faults.md §5](faults.md#5-waiting).

## Time

11. **No result depends on the frame size**, or on where a request starts or
    stops. Lengths and positions are counted in samples, or in main ticks,
    never in frames. Owner: this document.
12. **Times come from absolute targets.** A position, a deadline or a length
    is computed from an absolute origin and re-anchored at each step, never
    accumulated from converted or rounded per-step differences. Owner: this
    document.

## Construction and lifetime

13. **Errors in a script are found when it is evaluated.** A parameter that
    cannot be honoured fails at evaluation, as an invalid-value error
    attached to the argument it names, never discovered lazily while
    streaming. Owner: this document; the classes are
    [faults.md §2](faults.md#2-classes).
14. **Every check before any effect.** An operation that can fail checks
    everything first, so that a failure leaves things exactly as they were.
    Owner: this document.
15. **A list never keeps alive what it lists.** A registry, a notification
    list or a cache does not hold a stopped clock, a released source or a
    closed connection. A holder of a source keeps nothing of it once it
    left, whatever the way. Owner: this document.
16. **A component reads a setting when it is built**, unless the setting's
    owner states otherwise. Owner: this document.
17. **Every resource is released exactly once**, on every path, including a
    failure part-way. Owner: [concurrency.md](concurrency.md#4-resources).

## Callbacks and concurrency

18. **Callbacks.** When they fire, in which order, what `synchronous`
    means, and what a raising handler does. Owner:
    [source-contract.md §10](../core/source/spec/source-contract.md#10-callbacks).
19. **The streaming thread never waits for a peer**, and no user code runs
    under an operator lock. Owner: [concurrency.md](concurrency.md).

## Verification

20. **A check counts once it was seen to fail.** Owner:
    [testing.md](testing.md).
