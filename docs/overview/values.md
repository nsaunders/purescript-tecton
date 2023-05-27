# Values

## Lists

Lists can be found throughout CSS, notably in [selectors](./selectors.md) and many properties such as `transitionProperty` (which accepts a list of properties to animate). Because they are often overloaded with different types of values, Tecton uses a list syntax based on [nested tuples](https://pursuit.purescript.org/packages/purescript-tuples/4.0.0/docs/Data.Tuple.Nested#v:(/\\)) that supports heterogeneous data. Here is how the syntax looks:

```haskell
module Example.StyleSheet where

import Data.Tuple.Nested ((/\))
import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  a /\ button /\ summary ? Rule.do
    transitionProperty := color /\ backgroundColor
    transitionDuration := ms 150 /\ ms 75
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4YZgGcAxilpgpAgCQBKVQB4AfBo0s2HRADNTMeZQEJzlvIdbp2lACq0CnCPRzzazdGt6EX5aDTgUWTh3T28YX39Ao2cTUVkUCywAYRAYWnlBCBF0ixy8gsNklzgAERQ8FCxXCEwYLAA5GFk8GDA4AQEAegAdAyqTV3sQEXG3KZEsACVoeEi4ZdgmbqJ4sgo4AC4DuCycHAZt4j3KAF5pEAY4CLgRuAAjCDw8aZfhuFkIMBgEoCHAAPzrFZYMAPJ5PPC0FDFcTMabcEDoPKEQ53eQgKDsX7vFDyADWAHNaCAimAcgTaI84QikbIUdMat56qiRDi4MAogBGACsAAYify4AB2IVMYHMHlHMwWKxwUTiBhynk3RkxLw+PwBPBYYKhRlPNRwIolGBlfKMOFwC1wABE2nQ9F02jx-F0zrgejgJrypHIlHdFGxl12of9uhd2kG3pgnsG4d9QA)

## Multipart values

With tuples reserved for [lists](#lists), Tecton uses its own `Pair` type, constructed via the `~` operator, to represent multipart values that would typically be separated by spaces in vanilla CSS. Examples of this can be found in shorthand syntax, multiple-keyword values (as in [`alignSelf`](https://github.com/nsaunders/purescript-tecton/search?q=path%3Atest+alignSelf+first+baseline)), and two-part X/Y values (as in [`backgroundSize`](https://github.com/nsaunders/purescript-tecton/search?q=path%3Atest+backgroundSize+pct+px)).

A common example of this is setting a padding value for each of an element's four sides using a single declaration:

```haskell
module Example.InlineStyle where

import Tecton

inlineStyle :: Declarations _
inlineStyle = padding := nil ~ pct 10 ~ px 20 ~ em 5
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoCSA7KASzxgGUAXAT1jgHcALGAJxgChXCMQny4AFFlAhh4ACgCqeQuQA0cUQBIAlHNEAeAHxKlHLj0QAzAzADGvUQiOnyOzum68AKk0p8ILUiaaF05lnhEmJTgUAGc4Z1d3Mi8fcl17fUlQlGMsAGEQZhMxCDwU40zsmB0EhwjrEDx2YiISCmp4AC4muAARUygUJhRyQirwgH0OAmIyKhoAXjh0FDAwYgBzOCbpqSg4AD8ZszgARgAGLZmkOAAmI+2YYDgAVnZgFGIV1stjXclpVkfnydY4CIuNweWK+LD+QL-AFwBRwPIFGBFJg5KEA2FwABEanQLA0ahM4BgGgxcE0cAhzHwdXgtTGDRoZKxAHoCSI8UycUSMUA)

## Measures

The `Measure` type provides a common interface for many types of values, such as lengths, angles, and durations, allowing them to be calculated and/or combined via [calc expressions](./calc.md). It is a [phantom type](https://jordanmartinez.github.io/purescript-jordans-reference-site/content/31-Design-Patterns/03-Phantom-Types/01-What-Are-Phantom-Types.html), meaning that each value is tagged as a `Length`, `Percentage`, `Time`, etc. This tag ensures that incompatible values can't be combined (such as a `Measure Time` with a `Measure Length`). It also prevents the wrong kind of value from being assigned to a given property, e.g. a percentage value to a property that only supports a length.

You can construct a measure by applying the appropriate function (named for the corresponding CSS unit), followed by a number or integer, e.g. `px 100` (rendered as `100px`). The following table lists the various types of measures along with the functions that can be used to construct their values.

| Measure type | Constructors |
| ------------ | ------------ |
| `Length`     | `ch`, `em`, `ex`, `rem`, `vh`, `vw`, `vmin`, `vmax`, `px`, `cm`, `mm`, `pc`, `pt`, `inch` |
| `Percentage` | `pct` |
| `Angle` | `deg`, `rad`, `turn` |
| `Time` | `ms`, `sec` |
| `LengthPercentage` | via [calc expressions](./calc.md) |
| `Nil` | `nil` |

In some cases, measures of different types can be used interchangeably. For example, `nil` produces a value that can be used as a length, percentage, angle, or time. Similarly, values of type `Measure Length` or `Measure Percentage` can be used wherever a `Measure LengthPercentage` is expected (although the opposite is not true).

## Colors

Tecton is compatible with the [`Color` type from the _colors_ package](https://pursuit.purescript.org/packages/purescript-colors/7.0.1/docs/Color#t:Color), along with the following CSS-specific color keywords:
* `transparent`
* `currentColor`

## CSS-wide keywords

A few global keywords can be assigned to any property. In Tecton, these are
* `initial`, which resets the property to its default value defined in the CSS specification;
* `inherit`, which uses the property value inherited from the parent element; and
* `unset`, which either inherits the value of an inheritable property or reverts to the initial value.

Notice how `inherit`, for example, is compatible with each property:

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
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoCSA7KASzxgGUAXAT1jgHcALGAJxgChXCMQny4AFFlAhh4ACgCqeQuQA0cUQBIAlHNEAeAHxKlHLj0QAzAzADGvUQiOnyOzum68AKk0p8ILUiaaF05lnhEmJTgUAGc4Z1d3Mi8fcl17fUlQlGMsAGEQZhMxCDwU40zsmB0EhwjrEDwy-UdKvCwAJWh4MLhm2HZiIhIKangALgG4ABFTKBQmFHJCKvCAfQ4CYjIqGgBedpasMBBWODgAIxQTAGsAcyYQPLBMqG44Ac3iRm94g5MQe6ZH57xX6T7OAGKrkUiEABegz+APecGAk3OxF+cBezEBB2u5B6q36KLRb3YCORQ0MxjMcEkgOJeDg6yBkTcHliviw-kCQIOCjgeQKMCKTBynLg3LgACI1OgWBo1J8RBoxXBNHB2cx8DjUctemt4MqJQB6OUwGX6qXGsVAA)