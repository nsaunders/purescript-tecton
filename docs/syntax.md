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

Statements are the building blocks of a style sheet and are used to create basic rulesets associating a given selector with a group of declarations; [media queries](https://developer.mozilla.org/en-US/docs/Web/CSS/@media) adding conditional logic to nested statements; [font face](https://developer.mozilla.org/en-US/docs/Web/CSS/@font-face) rules defining custom fonts; and [keyframes](https://developer.mozilla.org/en-US/docs/Web/CSS/@keyframes) animations.

The `?` operator constructs a statement and is analogous to the curly braces that appear in CSS syntax, e.g.

```purescript
media all {} ?
  universal ? Rule.do
    width := nil
```
which is rendered as
```css
@media all {
  * {
    width: 0;
  }
}
```

#### Rulesets

##### Selectors

TODO

##### Declarations

A [declaration](https://developer.mozilla.org/en-US/docs/Web/CSS/Syntax#css_declaration_blocks) assigns a value to a given CSS property. Declarations are constructed using the `:=` operator, e.g.

```purescript
margin := nil
```

#### `@media` rules

TODO

#### `@font-face` rules

TODO

#### `@keyframes` rules

TODO

Common values
-------------

TODO

Properties
----------

TODO
