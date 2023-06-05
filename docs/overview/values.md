# Values

## Lists

Lists can be found throughout CSS, notably in [selectors](./selectors.md) and many properties such as [`transitionProperty`](./animations.md#transitions) (which accepts a list of properties to animate). Because they are often overloaded with different types of values, Tecton uses a list syntax based on [nested tuples](https://pursuit.purescript.org/packages/purescript-tuples/4.0.0/docs/Data.Tuple.Nested#v:(/\\)) that supports heterogeneous data. Here is how the syntax looks:

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

## Positions

Position values are used in background positioning, transforms, and gradients. A position consists of one of the following:

* `center`
* `top`
* `right`
* `bottom`
* `left`
* X/Y keyword positions, e.g. `left ~ top`
* single length-percentage value, e.g. `px 25` or `pct 50`
* X/Y length-percentage values, e.g. `pct 10 ~ px 25`

## URLs

URLs have several use cases in CSS, including images, fonts, and custom cursors. To construct a `URL` value, simply apply the `url` function to a URL string, e.g.

```haskell
module Example.InlineStyle where

import Tecton

inlineStyle :: Declarations _
inlineStyle = backgroundImage := url "./bg.gif"
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoCSA7KASzxgGUAXAT1jgHcALGAJxgChXCMQny4AFFlAhh4ACgCqeQuQA0cUQBIAlHNEAeAHxKlHLj0QAzAzADGvUQiOnyOzum68AKk0p8ILUiaaF05lnhEmJTgUAGc4Z1d3Mi8fcl17fUlQlGMsAGEQZhMxCDwU40zsmB0EhwjrEDx2YiISCmp4AC4muAARUygUJhRyQirwgH0OAmIyKhoAXjgAIxQTAGsAcyYQPLAcYBQl5un3KDgAIiwAehmlrCXCA0P2LeI4FsNjMzhJaVZ7vDhJ1jgIlxuDyxXxYfyBP7-OAKOB5AowIpMHKQ-4wo5qdAsDRqEzgGAaQ5wTRwcHMfB1eC1MYNGjEw5qE64kTYk6Y-GHIA)

## Images

Images can be used for backgrounds, custom list markers, and more. An image consists of either a [`URL` value](#urls) or a [gradient](#gradients).

### Gradients

#### Linear gradients

A linear gradient can be constructed using the `linearGradient` function, parameterized by an [angle](#measures) and a [color stop list](#color-stop-lists).

For example, the following declaration creates a background image that transitions top to bottom from black to white:

```haskell
module Example.InlineStyle where

import Color (black, white)
import Data.Tuple.Nested ((/\))
import Tecton

inlineStyle :: Declarations _
inlineStyle = backgroundImage := linearGradient (deg 180) (black /\ white)
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoCSA7KASzxgGUAXAT1jgHcALGAJxgChXCMQny4AFFlAhh4ACgCqeQuQA0cUQBIAlHNEAeAHxKlHLj0QAzAzADGvUQiOnyOzum68AKk0p8ILUiaaF05lnhEmJTgUAGc4Z1d3Mi8fcl17fUlQlGMsAGEQZhMxCDwU40zsmB0EhzhMqG55ACMoFBMAazkGaRKy-QARFHIULEcITBgsADkYUPIYMHlRAHoAHW0Op2sQPHZiIhIKangALj24TtN6ph7CNfCAfQ4CYjIqGgBeOBqGxoBzJhA8sBxgFAffYvLYwFBMADiZzAhBgeHMIg+cAAjAAOAAMwVEdXecAWdHobVKAOIcAOhmMZjgkmkrBJeDgT1YcAiLjcHliviw-kCzJZcAUcDyBRgRSYOT5LMFcAARGp0CwNGoTOAYBoZXBNHAecx8KC4Jt7jsaFq5bMVSIlbMFWqZUA)

#### Radial gradients

You can create a radial gradient using the `radialGradient` function.

The first parameter accepts any of the following:
* A shape and extent (e.g. `circle ~ closestSide` or `ellipse ~ farthestCorner`)
* A length (e.g. `px 100`), representing a circle radius
* A pair of length-percentage values, representing the horizontal and vertical radii of an ellipse

The second parameter accepts the [position](#positions) of the center point of the gradient.

The third parameter accepts a [color stop list](#color-stop-lists).

#### Color stop lists

A color stop [list](#lists) defines the colors and their respective positions within a gradient. Each color stop consists of a color and an optional length-percentage position, e.g. `rgb 255 0 0` or `black ~ pct 50`. A length-percentage transition hint may optionally be inserted between two color stops to set the midpoint in the color transition.

The following gradient demonstrates all of these options:

```haskell
module Example.InlineStyle where

import Color (rgb)
import Data.Tuple.Nested ((/\))
import Tecton

inlineStyle :: Declarations _
inlineStyle =
  backgroundImage :=
    linearGradient nil $
      rgb 0 160 0 /\ rgb 0 100 0 ~ pct 75 /\ pct 80 /\ rgb 135 206 235
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoCSA7KASzxgGUAXAT1jgHcALGAJxgChXCMQny4AFFlAhh4ACgCqeQuQA0cUQBIAlHNEAeAHxKlHLj0QAzAzADGvUQiOnyOzum68AKk0p8ILUiaaF05lnhEmJTgUAGc4Z1d3Mi8fcl17fUlQlGMsAGEQZhMxCDwU40zsmB0EhzhMqG55JgBzACNbPV4AERRyFCxHCEwYLAA5GFDyGDB5UQB6AB1tMv1HaxA8dmIiEgpqeAAuLbgW0ygUJnbCJfCAfQ4CYjIqGgBeVjg4epQTAGtaphA8sBxgFC1baPZ7PNYwI4AcWOYEIMDwvCkUDgCieoOedXqcAADHAAIwANlxuOmcExOPx2OJcAAfnB0GY4AB2ACscFJDN4AA4SVMyQ18QBmNkAJmxBLgIuF7ABxDgO0MxkZkmkrFleDgIIiLjcHliviw-kCaOeCjgeQKMCKTByJpRzwARGp0CwNGoTOAYBoHXBNGT4YF8OC4KsbhsaH6nRMPSI3RMXV6HUA)

#### Repeating gradients

To create a repeating linear or radial gradient, simply apply the `repeating` function to the gradient, e.g.

```haskell
module Example.InlineStyle where

import Color (black, white)
import Data.Tuple.Nested ((/\))
import Tecton

inlineStyle :: Declarations _
inlineStyle = backgroundImage := repeating $ linearGradient nil $ black /\ white
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoCSA7KASzxgGUAXAT1jgHcALGAJxgChXCMQny4AFFlAhh4ACgCqeQuQA0cUQBIAlHNEAeAHxKlHLj0QAzAzADGvUQiOnyOzum68AKk0p8ILUiaaF05lnhEmJTgUAGc4Z1d3Mi8fcl17fUlQlGMsAGEQZhMxCDwU40zsmB0EhzhMqG55ACMoFBMAazkGaRKy-QARFHIULEcITBgsADkYUPIYMHlRAHoAHW0Op2sQPHZiIhIKangALj24TtN6ph7CNfCAfQ4CYjIqGgBeOBqGxoBzJhA8sBxgFAffYvFjoGDnPAfOAKOBbcFMADiZzAhBgeF4Uig0Ne9SacAWdHobXYAOIcAOhmMZjgkmkrFJeDgT1YcAiLjcHliviw-kCLNZ2LyBRgRSYOX5rJhcAARGp0CwNGoTOAYBppXBNHBecx8HC4Jt7jsaJrZbNlSJFbN5arpUA)

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