# Fallback (Part A)

Policy: play the first available child. Sections follow
[policies.md](policies.md).

## 1. Children

`fallback(~id, sources)`. No `weight`.

## 2. State

None.

## 3. Predicates

Every predicate holds, and is known to hold without being evaluated
([switch.md §9](switch.md#9-other-properties)).

## 4. Events

None.

## 5. Shape

One `switch`. List order is priority.

A fallback with an infallible child that is not `single` is infallible.

Whether a higher-priority child takes over in the middle of a track is
decided by track sensitivity alone ([switch.md §4](switch.md#4-decisions)).
