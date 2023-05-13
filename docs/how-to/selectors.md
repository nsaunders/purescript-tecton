# Selectors

A selector is a pattern that describes the scope of a rule, i.e. the set of HTML elements to which it should apply. Tecton supports several types of selectors, including element, class, ID, attribute, and pseudo-class selectors. You can combine these to form more advanced selectors or even lists of selectors.

## Element selectors

An element selector targets all HTML elements of a particular type, such as `div`, `p`, `a`, or `img`. In its most basic form, an element selector simply consists of the target element type, e.g.

```haskell
module Example.StyleSheet where

import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  a ? Rule.do -- Applies to HTML `a` (anchor) elements.
    textDecorationLine := none
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4AgCQBKKQIA8APjlyWbDogBmOmAGNKAhHsN4NrdO0oAVWgU4R6OA7Wbpj9Ef1py4KADOcPaOzjCu7p6a1tqigSj6WADCIDC0BoIQIgn6qemZGjE2IeYgIsXatmUiWABK0PBBcA2wTIGExGQUcABcvXDJODgMHUQR3ZQAvHBgIAxwAXAA-C2NWHNwALRbcACC6JjMMMF4IHAAErYAsgAycAAGKA-SKCIGJOz+MLDAMCJ4QJYBaLOB4GBIPAAEUM7BQeGY5VuzBE8F6MxE5UYDGAKBRfQGpn0RjgonEOLxIjgUxBoScLjcHjwWG8vhBixkcGyuRg+QyjFBcE5cAAREp0PQVEoDOAYCoRXBVHBWelSORKBKKIQ4GMuurFSpRUoAPQy-hS42a+VAA)

### Universal selector

The _universal selector_ is a special type of element selector that applies to any element. In CSS, it is denoted by the `*` symbol. In Tecton, it is called `universal`. Any selector that does not target another type of element must begin with the universal selector.

> **Note**
> If you have written CSS before, you have almost certainly used the universal selector, perhaps without realizing: It is implicitly included in other types of selectors; for example, `.foo` is actually the same as `*.foo`. For more information, see [Selectors Level 3 (W3C)](https://www.w3.org/TR/selectors-3/#universal-selector).

The following rule uses the universal selector to remove the margin from every element in the webpage:

```haskell
module Example.StyleSheet where

import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  universal ? Rule.do
    margin := nil
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4AgCQBKKQIA8APjlyWbDogBmOmAGNKAhHsN4NrdO0oAVWgU4R6OA7Wbpj9Ef1py4KADOcPaOzjCu7p6a1tqigSj6WADCIDC0BoIQIgn6qemZGjE2IeYgIsXatmUiWABK0PBBcA2wTIGExGQUcABcvXDJODgMHUQR3ZQAvHBgIAxwcNnMAG7pCVBwAPwtjVhzC4twwCi0AObMIn0zYlBMJ5d9A6b6RnCi4gwPV1OHoU4uNwePBYby+Q6LGRLHKJGD5DKMI5wKFwABESnQ9BUSgM4BgKlRcFUcDB6VI5EomIohDgYy6FKJKjRSgA9Lj+NiWVSCUA)

## Class selectors

A class selector matches any element whose `class` attribute contains the specified class name. The `&.` operator appends the target class to a selector. For example, the following selector targets all elements with the class name _container_:

```haskell
module Example.StyleSheet where

import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  universal &. ClassName "container" ? Rule.do
    width := auto
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4AgCQBKKQIA8APjlyWbDogBmOmAGNKAhHsN4NrdO0oAVWgU4R6OA7Wbpj9Ef1py4KADOcPaOzjCu7p6a1tqigSj6WADCIDC0BoIQIgn6qemZGjE2IeYgIsXatmUiWABK0PBBcA2wTIGExGQUcABcvXDJODgMHUQR3ZQAvHBgIAxwcNnMAG7pCVBwAGRYg1BBgQByaPAARAbleCjMIumncAD8LY1YcwuL1MxgeCR9MygQPDzBjAa4iPoDUz6IxwUTiEFguBTd6hJwuNwePBYby+d6LGRLHKJGD5DKMD5wAlwU5KdD0FRKC78FT3VRwHHpUjkSh0iiEOBjLrcuBsmkAeiZMAZYt5LKAA)

## ID selectors

An ID selector targets the element whose `id` attribute is exactly the specified value. The `&#` operator appends the element ID to another selector. The selector in the following example matches the element with ID _appBar_:

```haskell
module Example.StyleSheet where

import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  universal &# Identifier "appBar" ? Rule.do
    height := px 64
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4AgCQBKKQIA8APjlyWbDogBmOmAGNKAhHsN4NrdO0oAVWgU4R6OA7Wbpj9Ef1py4KADOcPaOzjCu7p6a1tqigSj6WADCIDC0BoIQIgn6qemZGjE2IeYgIsXatmUiWABK0PBBcA2wTIGExGQUcABcvXDJODgMHUQR3ZQAvHBgIAxwcNnMAG7pCVBwAGQAxHAAkvwieMw6zOlwAEQo6OgAQii0l3AA-C2NWHMLi3BkzADmJEovRm6CQcAAbAAWJjAFDMER9AamfRGOCicQMOEIuBTb6hJwuNwePBYby+b6LGRLHKJGD5DKMH5walXJToegqJQGcAwFTPVRwcnpUjkSgciiEOBjLpiuCCy5KAD0PP4XKVEv5QA)

## Attribute selectors

An attribute selector targets elements based on their attributes or attribute values. It can match on the presence of any HTML attribute such as `href`, `src`, or `alt`. A number of operators can further limit matches to specific attribute values.

### Presence

To match elements based on the presence of an attribute, the attribute is used by itself without any additional operator. For example, the following selector applies to an `input` element with a `required` attribute:

```haskell
module Example.StyleSheet where

import Color (rgb)
import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  input &@ required ? Rule.do
    backgroundColor := rgb 255 153 0
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4AgCQBKKQIA8APjlyWbDogBmOmAGNKAhHsN4NrdO0oAVWgU4R6OA7Wbpj9Ef1py4KADOcPaOzjCu7p6a1tqigSj6WADCIDC0BoIQIgn6qemZGjE2cKlQ7NK0AOYARpZaduYgIsXatk0iWABK0PBBcD2wTIGExGQUcABck6U4OAwjRBHjlAC8cGAgDHBwzCLoEJQAZAACcPQAjhDM9GBwAPwDvVib2ztwNSgGANZVtCDZMBlCqTdbVGpwABMAFZoXAAIzQgDMcAADExgCg9lMZqZ9EY4KJxAxMdjVm9Qk4XG4PHgsN5fG8djI4NlcjB8hlGO84Cy4AAiJToegqJQGcAwFT8uCqc4wHzpUjkSjCiiEOCLMbKmUqAVKAD04v4ov1qqlQA)

### Exact value 

The `@=` operator can be used to target an element whose attribute value exactly matches the specified value. For example, the following selector matches elements with the attribute `role="button"`:

```haskell
module Example.StyleSheet where

import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  universal &@ role @= "button" ? Rule.do
    fontWeight := bold
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4AgCQBKKQIA8APjlyWbDogBmOmAGNKAhHsN4NrdO0oAVWgU4R6OA7Wbpj9Ef1py4KADOcPaOzjCu7p6a1tqigSj6WADCIDC0BoIQIgn6qemZGjE2IeYgIsXatmUiWABK0PBBcA2wTIGExGQUcABcvXDJODgMHUQR3ZQAvHBgIAxwcNnMAG7pCVBwAGQAAnC0ILBwOzMARABGEHh45adwAPwtjVhzC4twOuV4AOowzADmJEovRm50OYCYwBQzBEfQGpn0RjgonEDChMLgUzeoScLjcHjwWG8vjeixkSxyiRg+QyjHecHJcFOSnQ9BUSgM4BgKjuqn2MB86VI5EorIohDgYy6IrgfOZAHpOfx2fKxTygA)

### Value within list

The `~=` operator can be used to find a matching value within a whitespace-delimited list of words. In the following example, the selector will match an element with an attribute `class="foo bar baz"`. (This is, of course, equivalent to a [class selector](#class-selectors).)

```haskell
module Example.StyleSheet where

import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  universal &@ class' ~= "bar" ? Rule.do
    display := none
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4AgCQBKKQIA8APjlyWbDogBmOmAGNKAhHsN4NrdO0oAVWgU4R6OA7Wbpj9Ef1py4KADOcPaOzjCu7p6a1tqigSj6WADCIDC0BoIQIgn6qemZGjE2IeYgIsXatmUiWABK0PBBcA2wTIGExGQUcABcvXDJODgMHUQR3ZQAvHBgIAxwcNnMAG7pCVBwAGQAAnAGUEGBAORwAH4zAEQARii0l3AA-C2NWHMLi7PMgZgoBH0zETlRgMYAoZgiPoDUz6IxwUTiUHgyFTD6hJwuNwePBYby+D6LGRLHKJGD5DKMT5wIlwS5KdD0FRKAzgGAqB6qOB49KkciUBkUQhwMZdPlwTl0gD0LP4TMlAvZQA)

### Code/subcode

Intended primarily for locale tags, the `|=` operator matches when an attribute value
- exactly matches the specified value or
- begins with the specified value followed by a hyphen.

The selector in the following example will match an `a` element whose `hreflang` attribute is equal to `"en"` or `"en-US"`, but not `"end"`:

```haskell
module Example.StyleSheet where

import Color (rgb)
import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  a &@ hreflang |= "en" ? Rule.do
    color := rgb 255 0 196
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4AgCQBKKQIA8APjlyWbDogBmOmAGNKAhHsN4NrdO0oAVWgU4R6OA7Wbpj9Ef1py4KADOcPaOzjCu7p6a1tqigSj6WADCIDC0BoIQIgn6qemZGjE2cKlQ7NK0AOYARpZaduYgIsXatk0iWABK0PBBcD2wTIGExGQUcABck6U4OAwjRBHjlAC8cGAgDHABcABkAAJwJPQ6UCgiVXAAPusARDAid3AA-AO9WJvbO3AGIOW0Kbrao1OAAJgArBC4AAGOAARgAnAA2JjAFDMERTGamfRGOCicQMdGYuCrb6hJwuNwePBYby+b47GRwbK5GD5DKMH5wFlwO5KdD0FRKP78FTPVRwBnpUjkShCiiEOCLMbyuBSgUAejFMBFWsVEqAA)

### Prefix

You can use the `^=` operator to represent an element whose attribute value begins with the specified value. The following example targets insecure links based on their _http://_ prefix:

```haskell
module Example.StyleSheet where

import Color (rgb)
import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  a &@ href ^= "http://" ? Rule.do
    color := rgb 255 0 0
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4AgCQBKKQIA8APjlyWbDogBmOmAGNKAhHsN4NrdO0oAVWgU4R6OA7Wbpj9Ef1py4KADOcPaOzjCu7p6a1tqigSj6WADCIDC0BoIQIgn6qemZGjE2cKlQ7NK0AOYARpZaduYgIsXatk0iWABK0PBBcD2wTIGExGQUcABck6U4OAwjRBHjlAC8cGAgDHABcABkAAJwJPQ6cAB66wBEJHh46JMA9I9XcAD8A71Ym9s7cAYgcq0Kbrao1OAAJgArFC4AAGeFMYAoZgiKYzUz6IxwUTiBjI1FwVa-UJOFxuDx4LDeXy-HYyODZXIwfIZRh-OAMuBXJToegqJQA-gqV6qOA09KkciUPkUQhwRZjaVwMU8x5CmACx6ykVAA)

### Suffix

The `$=` operator matches an element with an attribute value ending in the specified value. This can be used to target a link to a PDF, for instance:

```haskell
module Example.StyleSheet where

import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  a &@ href $= ".pdf" ? Rule.do
    fontWeight := bold
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4AgCQBKKQIA8APjlyWbDogBmOmAGNKAhHsN4NrdO0oAVWgU4R6OA7Wbpj9Ef1py4KADOcPaOzjCu7p6a1tqigSj6WADCIDC0BoIQIgn6qemZGjE2IeYgIsXatmUiWABK0PBBcA2wTIGExGQUcABcvXDJODgMHUQR3ZQAvHBgIAxwAXAAZAACcCT0OnAyMwBEWOhgOntwAPwtjVhzC4twOuV4AOowzADmJJS9MwBGIFBgJjAFDMER9AamfRGOCicQMYGguBTW6hJwuNwePBYby+W6LGRwbK5GD5DKMO47RZ7JToegqJQGcAwFSnVRwHHpUjkSi0iiEOBjLrcuBs6kAekZ-HpYt5LKAA)

### Substring

To target an element whose attribute value simply contains a given substring, use the `*=` operator. The following selector matches any image sourced from Wikimedia:

```haskell
module Example.StyleSheet where

import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  img &@ src *= "wikimedia.org" ? Rule.do
    display := initial
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4AgCQBKKQIA8APjlyWbDogBmOmAGNKAhHsN4NrdO0oAVWgU4R6OA7Wbpj9Ef1py4KADOcPaOzjCu7p6a1tqigSj6WADCIDC0BoIQIgn6qemZGjE2IeYgIsXatmUiWABK0PBBcA2wTIGExGQUcABcvXDJODgMHUQR3ZQAvHBgIAxwcKwA5nAAZAACcIEZcABUMwBEVMwA1qwwYMwoWOzLh3AA-C2NWHMLi7PMgZgoBH0zZhiPDXKBMYAoIF9AamfRGOCicQMCFQqYfUJOFxuDx4LDeXwfRYyODZXIwfIZRifODEuCHJToegqJQGcAwFQPVRwfHpUjkSiMiiEbadCb8uBc+kAelZ-GZUsFHKAA)

## Pseudo-classes

A pseudo-class is a keyword added to a selector to style elements based on their state or position in the DOM tree. You can append a pseudo-class to the end of a selector using the `&:` operator. For instance, the `focus` pseudo-class applies a style conditionally when the element has focus:

```haskell
module Example.StyleSheet where

import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  universal &: focus ? Rule.do
    outlineStyle := solid
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4AgCQBKKQIA8APjlyWbDogBmOmAGNKAhHsN4NrdO0oAVWgU4R6OA7Wbpj9Ef1py4KADOcPaOzjCu7p6a1tqigSj6WADCIDC0BoIQIgn6qemZGjE2IeYgIsXatmUiWABK0PBBcA2wTIGExGQUcABcvXDJODgMHUQR3ZQAvHBgIAxwcNnMAG7pCVBwAGQDOiAGEMEA-C2NWHMLi3AgEHhQzCIRnfC9M4Eg92BMwCgPfQOmfRGOCicQMH5-KaXUJOFxuDx4LDeXyXRYyJY5RIwfIZRhXODouAAIiU6HoKiUBnAMBURLgqjgyPSpHIlDJFEIcDGXVZ9JUxKUAHoqfwKYL2bSgA)

<!-- TODO: Create a "Basic pseudo-classes" section that lists and describes all of the supported pseudo-classes that don't have special syntax. -->

### Nth pseudo-classes

A special group of pseudo-classes allows you to select an element by its position within the parent element. These use a formula of the form **a**_n_+**b** where:

- **a** determines the frequency of matches. For example, **3**_n_ can be interpreted as _every third element_.
- **b** represents an offset, i.e. the index of the first matching element (one-indexed). For example, **2** will match the second element, before the pattern repeats.

These pseudo-classes are
- `nthChild`, which selects elements matching the **a**_n_+**b** formula within their parent;
- `nthLastChild`, which reverses the direction of `nthChild`, counting from the last child element within a container;
- `nthOfType`, which is like a version of `nthChild` that ignores sibling elements of a different type when evaluating the **a**_n_+**b** formula; and
- `nthLastOfType`, which reverses the direction of `nthOfType`, counting from the last child element within a container.

The [:nth Tester (CSS-Tricks)](https://css-tricks.com/examples/nth-child-tester/) offers an interactive way to learn how these pseudo-classes and the **a**_n_+**b** formula work.

Constructing these pseudo-classes in Tecton begins with the formula:

- The `even` function will select even-numbered elements, equivalent to **2**_n_.
- The `odd` function will select odd-numbered elements, equivalent to **2**_n_+**1**.
- The `#+` operator has two parameters **a** and **b** which are used to create a **a**_n_+**b** formula, e.g. `2 #+ 1`.
- The `#-` operator has two parameters **a** and **b** which are used to create a **a**_n_-**b** formula, e.g. `2 #- 1`.

Apply one of the `nth*` functions listed above to the result to create the pseudo class.

Here is how `nthChild`, for instance, can be used to add zebra-striping to a table:

```haskell
module Example.StyleSheet where

import Color (rgb)
import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  tr &: nthChild even ? Rule.do
    backgroundColor := rgb 240 240 240
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4AgCQBKKQIA8APjlyWbDogBmOmAGNKAhHsN4NrdO0oAVWgU4R6OA7Wbpj9Ef1py4KADOcPaOzjCu7p6a1tqigSj6WADCIDC0BoIQIgn6qemZGjE2cKlQ7NK0AOYARpZaduYgIsXatk0iWABK0PBBcD2wTIGExGQUcABck6U4OAwjRBHjlAC8cGAgDHBweLRwAGQzIngkySTMUGBwMABuMCJwAPwDvVib2ztwNSgGANZVWggbJgMoVSbrao1OAAJgALAAGWGI5EIpjAFDMR7TXT6IxwUTiBgYrFwVafUJOFxuDx4LDeXyfHYyODZXIwfIZRhfOAsuAAIiU6HoKiUBnAMBU-LgqjgDPSpHIlGFFEIcEWYyVMpUAqUAHpxfxRXqVVKgA)

<!-- TODO pseudo-elements -->

<!-- TODO selector lists -->