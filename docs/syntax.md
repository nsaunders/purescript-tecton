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

With [`Data.Tuple.Tuple`](https://pursuit.purescript.org/packages/purescript-tuples/docs/Data.Tuple#t:Tuple) reserved for lists, a new data type [`Pair`](../search?q=data+Pair) represents tuples, such as the vertical and horizontal values in `padding` shorthand (`padding := px 4 ~ px 8`). The `~` operator should be used in lieu of the `Pair` constructor and works the same as [`/\`](https://pursuit.purescript.org/packages/purescript-tuples/docs/Data.Tuple.Nested#v:(/\\)) does in other contexts.

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

A [selector](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Selectors) is a query used to select the elements to which a group of [declarations](#declarations) will be applied. A selector primarily targets a certain type of element but can be narrowed based on the element's attributes, class list, pseudo classes, pseudo element, or relationship to another element.

#### The universal selector

The `universal` selector corresponds to `*` in CSS and is used to select any type of element. Many selectors use `universal` as a starting point and narrow their scope using classes, attributes, etc.

> ℹ️ **NOTE**: `*` is more widespread in CSS than it appears. A selector `.cls` actually implies `*.cls`.

#### Element selectors

An element selector targets a certain type of element (i.e. an HTML tag). Aside from functioning as a selector itself, it can be used as the starting point for other selectors instead of [the universal selector](#the-universal-selector). A selector is provided for each type of standard HTML element such as `button` or `input`.

#### ID selectors

The `&#` operator constructs an ID selector, which targets an element based on its `id` attribute. For example `universal &# "button1"` is rendered as `*#button1`.

#### Class selectors

The `&.` operator constructs a class selector, which targets an element based on an item of its class list (`class` attribute). For example `a &. "button"` is equivalent to `a.button` in CSS.

#### Attribute selectors

TODO

#### Pseudo-classes

TODO

#### Pseudo-elements

TODO

#### Context

TODO (ancestor, parent, general sibling, adjacent sibling, etc.)

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
