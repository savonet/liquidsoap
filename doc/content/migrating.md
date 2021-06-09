Migrating to a new Liquidsoap version
=====================================

In this page, we list the most common catches when migrating to a new version of
Liquidsoap.

2.0
---

- `input.http`: by default the source now has its own clock, which means that
  you might encounter new clock errors due to this. You can pass the argument
  `self_sync=true` in order to disable this and restore previous behavior.
