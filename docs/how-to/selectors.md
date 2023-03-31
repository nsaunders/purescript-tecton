# Selectors

A _selector_ is a pattern that describes the scope of a rule, i.e. the set of HTML elements to which it should apply. Tecton offers several types of selectors, including element, class, ID, attribute, and pseudo-class selectors. These can also be combined to form more advanced selectors or even lists of selectors.

## Element selectors

An element selector targets all HTML elements of a particular type, such as `div`, `p`, `a`, or `img`. In its most basic form, an element selector simply consists of the target element type, e.g.

```haskell
module Example.StyleSheet where

import Tecton

styleSheet :: CSS
styleSheet = do
  a ? Rule.do -- Applies to HTML `a` (anchor) elements.
    textDecorationLine := none
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4AgCQBKKQIA8APjlyWbDogBmOmAGNKAhHsN4NrdO0oAVWgU4R6OA7Wbpj9Ef1py4KADOcPaOzjCu7p6a1tqigSj6WADCIDC0BoIQIgn6qemZGjE2IeYgIkyBhMRkFHAAXPVwyTg4DFVEEbWUALxwYCAMcAFwAPxwAErQMFgDcAC083AAguiYzDDBeCBwABK2ALIAMnAABiin0igiBiTs-jCwwDAieIFYQ8NweDBIeAAihnYKDwzHKR2YIng9T6InKjAYwBQkIaTVM+iMcFE4kRyJEcB6n1CThcbg8eCw3l8n2GMjg2VyMHyGUYXzgdLgACIlOh6ColAZwDAVJy4Ko4FT0qRyJReRRCHAOjUZWKVFylAB6QX8fkauUioA)

### Universal selector

The _universal selector_ is a special type of element selector that applies to any element. In CSS, it is denoted by the `*` symbol. In Tecton, it is called `universal`. Any selector that does not target another type of element must begin with the universal selector.

> **Note**
> If you have written CSS before, you have almost certainly used the universal selector, perhaps without realizing: It is implicitly included in other types of selectors; for example, `.foo` is actually the same as `*.foo`. For more information, see [Selectors Level 3 (W3C)](https://www.w3.org/TR/selectors-3/#universal-selector).

The following rule uses the universal selector to remove the margin from every element in the webpage:

```haskell
module Example.StyleSheet where

import Tecton

styleSheet :: CSS
styleSheet = do
  universal ? Rule.do
    margin := nil
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4AgCQBKKQIA8APjlyWbDogBmOmAGNKAhHsN4NrdO0oAVWgU4R6OA7Wbpj9Ef1py4KADOcPaOzjCu7p6a1tqigSj6WADCIDC0BoIQIgn6qemZGjE2IeYgIkyBhMRkFHAAXPVwyTg4DFVEEbWUALxwYCAMcHDZzABu6QlQcAD8cABK0DBYA0PDcMAotADmzCINfWJQTJt7DU2m+kZwouIMp-s9a6FOLm4eeFjevmvDMiM5RIwfIZRjrOD-OAAIiU6HoKiUBnAMBUULgqjg33SpHIlDhFEIcA6NVx6JU0KUAHokfwEZT8aigA)

## Class selectors

A class selector matches any element whose `class` attribute contains the specified class name. The `&.` operator appends the target class to a selector. For example, the following selector targets all elements with the class name _container_:

```haskell
module Example.StyleSheet where

import Tecton

styleSheet :: CSS
styleSheet = do
  universal &. "container" ? Rule.do
    width := auto
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4AgCQBKKQIA8APjlyWbDogBmOmAGNKAhHsN4NrdO0oAVWgU4R6OA7Wbpj9Ef1py4KADOcPaOzjCu7p6a1tqigSj6WADCIDC0BoIQIgn6qemZGjE2IeYgIkyBhMRkFHAAXPVwyTg4DFVEEbWUALxwYCAMcHDZzABu6QlQcABkWHAARAbleCjMIukLcAD8cABK0DBYA0PD1MxgeCQNfSgQeIMMwGsiDU2m+kZwouJPL3A9U6hJwuNwePBYby+U7DGQjHKJGD5DKMM5wOGLJToegqJTLfgqLaqOBQ9KkciUbEUQhwDo1ClwYkLJQAenxMFxLKphKAA)

## ID selectors

An ID selector targets the element whose `id` attribute is exactly the specified value. The `&#` operator appends the element ID to another selector. The selector in the following example matches the element with ID _appBar_:

```haskell
module Example.StyleSheet where

import Tecton

styleSheet :: CSS
styleSheet = do
  universal &# "appBar" ? Rule.do
    height := px 64
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4AgCQBKKQIA8APjlyWbDogBmOmAGNKAhHsN4NrdO0oAVWgU4R6OA7Wbpj9Ef1py4KADOcPaOzjCu7p6a1tqigSj6WADCIDC0BoIQIgn6qemZGjE2IeYgIkyBhMRkFHAAXPVwyTg4DFVEEbWUALxwYCAMcHDZzABu6QlQcABkAMRwAEQo6OgAQii0i3AA-HAAStAwWANDw3BkzADmJJT1fehIcABsACxMwCjMIg1NpvpGOCicQMT7fOA9M6hJwuNwePBYby+M7DGQjHKJGD5DKMc5wNFLJToegqJQGcAwFTbVRwJHpUjkSjEiiEOAdGqMuA0xZKAD05P4pN5zKpQA)

## Attribute selectors

An attribute selector targets elements based on their attributes or attribute values. It can match on the presence of any HTML attribute such as `href`, `src`, or `alt`. A number of operators can further limit matches to specific attribute values.

### Presence

To match elements based on the presence of an attribute, the attribute is used by itself without any additional operator. For example, the following selector applies to an `input` element with a `required` attribute:

```haskell
module Example.StyleSheet where

import Color (rgb)
import Tecton

styleSheet :: CSS
styleSheet = do
  input &@ required ? Rule.do
    backgroundColor := rgb 255 153 0
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4AgCQBKKQIA8APjlyWbDogBmOmAGNKAhHsN4NrdO0oAVWgU4R6OA7Wbpj9Ef1py4KADOcPaOzjCu7p6a1tqigSj6WADCIDC0BoIQIgn6qemZGjE2cKlQ7NK0AOYARpZaduYgIkyBhMRkFHAAXN2lODgMbUQRnZQAvHBgIAxwcMwi6BCUAGQAAnD0AI4QzPRgcAD8cABK0DBY07NzcDUoBgDWVbQg2WBlFd2T1TVwAEwAVgBcAAjACAMxwAAMTGAKAWPT6pn0RjgonEDDhCPG11CThcbg8eCw3l81zmMjg2VyMHyGUYNzglLgACIlOh6ColAZwDAVCy4KpNjAfOlSORKByKIQ4MMOhLBSpWUoAPQ8-hclVS-lAA)

### Exact value 

The `@=` operator can be used to target an element whose attribute value exactly matches the specified value. For example, the following selector matches elements with the attribute `role="button"`:

```haskell
module Example.StyleSheet where

import Tecton

styleSheet :: CSS
styleSheet = do
  universal &@ role @= "button" ? Rule.do
    fontWeight := bold
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4AgCQBKKQIA8APjlyWbDogBmOmAGNKAhHsN4NrdO0oAVWgU4R6OA7Wbpj9Ef1py4KADOcPaOzjCu7p6a1tqigSj6WADCIDC0BoIQIgn6qemZGjE2IeYgIkyBhMRkFHAAXPVwyTg4DFVEEbWUALxwYCAMcHDZzABu6QlQcABkAAJwtCCwcHN9AEQARhB4eOXrcAD8cABK0DBYA0PDcDrleADqMMwA5iSU9X2by2BMwCjMEQNJqmfRGOCicQMf6AuA9a6hJwuNwePBYby+a7DGQjHKJGD5DKMG5wHFwdZKdD0FRKAzgGAqA6qRYwHzpUjkShUiiEOAdGqcuDMikAejp-BpIu5jKAA)

### Value within list

The `~=` operator can be used to find a matching value within a whitespace-delimited list of words. In the following example, the selector will match an element with an attribute `class="foo bar baz"`. (This is, of course, equivalent to a [class selector](#class-selectors).)

```haskell
module Example.StyleSheet where

import Tecton

styleSheet :: CSS
styleSheet = do
  universal &@ class' ~= "bar" ? Rule.do
    display := none
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4AgCQBKKQIA8APjlyWbDogBmOmAGNKAhHsN4NrdO0oAVWgU4R6OA7Wbpj9Ef1py4KADOcPaOzjCu7p6a1tqigSj6WADCIDC0BoIQIgn6qemZGjE2IeYgIkyBhMRkFHAAXPVwyTg4DFVEEbWUALxwYCAMcHDZzABu6QlQcABkAAJwBlBBgQDkcAB+fQBEAEYotNtwAPxwAErQMFgDQ8P9zIGYKAQNfSLljAzAKMwiDU2mfRGOCicRfH5-Hq3UJOFxuDx4LDeXy3YYyEY5RIwfIZRh3ODouDbJToegqJQGcAwFRHVRwZHpUjkSikiiEOAdGrMuB04kAekp-HJfNZNKAA)

### Code/subcode

Intended primarily for locale tags, the `|=` operator matches when an attribute value
- exactly matches the specified value or
- begins with the specified value followed by a hyphen.

The selector in the following example will match an `a` element whose `hreflang` attribute is equal to `"en"` or `"en-US"`, but not `"end"`):

```haskell
module Example.StyleSheet where

import Color (rgb)
import Tecton

styleSheet :: CSS
styleSheet = do
  a &@ hreflang |= "en" ? do
    color := rgb 255 0 196
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4AgCQBKKQIA8APjlyWbDogBmOmAGNKAhHsN4NrdO0oAVWgU4R6OA7Wbpj9Ef1py4KADOcPaOzjCu7p6a1tqigSj6WADCIDC0BoIQIgn6qemZGjE2cKlQ7NK0AOYARpZaduYgIkyBhMRkFHAAXN2lODgMbUQRnZQAvHBgIAxwAXAAZAACcCT0OlAoIlVwAD6TAEQwIgdwAPxTM3NzBiDltD2T1TVwAEwArO9wAAxwAIwATgAbExgChmCIen1TPojHBROIGGCIXBxrMQg4nC43B48FhvL50XMZHBsrkYPkMoxrnASXADkp0PQVEpbvwVKdVHACelSORKEyKIQ4MMOvy4FyGQB6NkwFlSwUcoA)