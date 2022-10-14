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

### Declarations

A [declaration](https://developer.mozilla.org/en-US/docs/Web/CSS/Syntax#css_declaration_blocks) assigns a value to a given CSS property. Declarations are constructed using the `:=` operator, e.g.

```purescript
margin := nil
```
which is rendered as
```css
margin: 0;
```

### Selectors

TODO

### Rulesets

TODO

### Statements

Statements are the building blocks of a style sheet, constructed using the `?` operator which is analogous to curly braces in CSS. [Rulesets](#rulesets) are one example of a statement. Other examples include [media queries](#media-rules), [font face](#font-face-rules) rules, and [keyframes](#keyframes-rules) animations.

The following example demonstrates how the `?` operator can be used to construct multiple types of statements:

```purescript
media all {} ?
  universal ? Rule.do
    width := nil
```

This is rendered as:

```css
@media all {
  * {
    width: 0;
  }
}
```

### `@media` rules

TODO

### `@font-face` rules

TODO

### `@keyframes` rules

TODO

Common values
-------------

TODO

Properties
----------

TODO
