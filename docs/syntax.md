Syntax
======

> ⚠️ **NOTE**: This document is a work in progress.

Foundations
-----------

### Lists

Lists are represented using [nested tuple syntax `(/\)`](https://pursuit.purescript.org/packages/purescript-tuples/7.0.0/docs/Data.Tuple.Nested#v:(/\\)). For example, a list of time elements written as `ms 150 /\ nil /\ sec 1`, would be rendered as `150ms, 0, 1s`. This encoding offers several benefits:
1. It supports heterogeneous lists.
2. Empty lists are unrepresentable.
3. Lists can be constrained in various ways (for example, alternating items).
