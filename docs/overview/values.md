# Values and units

## CSS-wide keywords

A few keywords can be used for any CSS property. In Tecton, these are
* `initial`, which resets the property to its default value defined in the CSS specification;
* `inherit`, which uses the property value inherited from the parent element; and
* `unset`, which either inherits the value of an inheritable property or reverts to the initial value.

Notice how `inherit`, for example, can be assigned to any property:

```haskell
module Example.InlineStyle where

import Tecton
import Tecton.Rule as Rule

inlineStyle :: Declarations _
inlineStyle = Rule.do
  backgroundColor := inherit
  color := inherit
  fontSize := inherit
  margin := inherit
  outlineStyle := inherit
  cursor := inherit
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoCSA7KASzxgGUAXAT1jgHcALGAJxgChXCMQny4AFFlAhh4ACgCqeQuQA0cUQBIAlHNEAeAHxKlHLj0QAzAzADGvUQiOnyOzum68AKk0p8ILUiaaF05lnhEmJTgUAGc4Z1d3Mi8fcl17fUlQlGMsAGEQZhMxCDwU40zsmB0EhwjrEDwy-UdKvCwAJWh4MLhm2HZiIhIKangALgG4ABFTKBQmFHJCKvCAfQ4CYjIqGgBedpasMBBWODgAIxQTAGsAcyYQPLBMqG44Ac3iRm94g5MQe6ZH57xX6T7OAGKrkUiEABegz+APecGAk3OxF+cBezEBB2u5B6q36KLRbyBJncoQeT1R-3R8VYCORQ0MxjMcEkgNpeDg6yBkTcHliviw-kCQIOCjgeQKMCKTBywrgorgACI1OgWBo1J8RBoFXBNHBBcx8DiKTi+jRdUqAPQamBqi0qm0KoA)

## The `Measure` type

The `Measure` type provides a common interface for many types of values, such as lengths, angles, and durations, allowing them to be calculated and/or combined via [calc expressions](./calc.md). It is a [phantom type](https://jordanmartinez.github.io/purescript-jordans-reference-site/content/31-Design-Patterns/03-Phantom-Types/01-What-Are-Phantom-Types.html), meaning that each value is tagged as a `Length`, `Percentage`, `Time`, etc. This tag ensures that incompatible values can't be combined (such as a `Measure Time` with a `Measure Length`). It also prevents the wrong kind of value from being assigned to a given property, e.g. a percentage value to a property that only supports a length.

