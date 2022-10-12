Syntax
======

> ⚠️ **NOTE**: This section is a work-in-progress. For now, you can reference the [examples](../examples) and [tests](../docs). Contributions are welcome!

Foundations
-----------

### Lists

Lists are represented using [nested tuple syntax `(/\)`](https://pursuit.purescript.org/packages/purescript-tuples/docs/Data.Tuple.Nested#v:(/\\)). For example, a list of time elements written as `ms 150 /\ nil /\ sec 1` would be rendered as `150ms, 0, 1s`. This encoding offers several benefits:
1. It supports heterogeneous lists.
2. Empty lists are unrepresentable.
3. Lists can be constrained in various ways (for example, alternating items).

### Tuples

With [`Data.Tuple.Tuple`](https://pursuit.purescript.org/packages/purescript-tuples/docs/Data.Tuple#t:Tuple) reserved for lists, a new data type [`Pair`](../search?q=data+Pair) represents tuples, such as the vertical and horizontal values in `padding` shorthand (`padding := px 4 ~ px 8`). The `(~)` operator should be used in lieu of the `Pair` constructor and works the same as [`(/\)`](https://pursuit.purescript.org/packages/purescript-tuples/docs/Data.Tuple.Nested#v:(/\\)) does in other contexts.

### Statements

#### `@media` rules

TODO

#### `@font-face` rules

TODO

#### `@keyframes` rules

TODO

#### Rulesets

##### Selectors

TODO

##### Declarations

TODO

Common values
-------------

TODO

Properties
----------

TODO
