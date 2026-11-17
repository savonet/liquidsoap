# Fallback (Part A, as-built)

Policy: play the first ready child. Sections follow
[policies.md](policies.md).

## 1. Children

`fallback(~id, sources)`. No `weight`.

## 2. State

None.

## 3. Predicates

Every predicate is the constant `true`.

## 4. Events

None.

## 5. Shape

One `switch`. List order is priority.

Because the predicates are literally `true`, a fallback with an infallible,
non-`single` child is infallible
([switch.md §11](switch.md#11-other-properties)).

Whether a higher-priority child takes over mid-track is decided by track
sensitivity alone ([switch.md §7](switch.md#7-search)).
