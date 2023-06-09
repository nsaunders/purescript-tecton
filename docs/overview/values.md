# Values and units

## Component values

Component values include keywords and various data types. A property value usually consists of a single component. In many cases, however, you may combine multiple components to form a complete property value. In CSS, these would typically be separated by spaces. With Tecton, you can use the `~` operator to join them together. For example:

```haskell
module Example.InlineStyle where

import Tecton
import Tecton.Rule as Rule

inlineStyle :: Declarations _
inlineStyle = Rule.do

  -- Setting top, x, and bottom padding in a single declaration
  padding := px 4 ~ px 8 ~ px 16

  -- Combining two keywords
  alignSelf := safe ~ center
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoCSA7KASzxgGUAXAT1jgHcALGAJxgChXCMQny4AFFlAhh4ACgCqeQuQA0cUQBIAlHNEAeAHxKlHLj0QAzAzADGvUQiOnyOzum68AKk0p8ILUiaaF05lnhEmJTgUAGc4Z1d3Mi8fcl17fUlQlGMsAGEQZhMxCDwU40zsmB0EhwjrEDwy-UdKvCwAJWh4MLhm2HZiIhIKangALgG4ABFTKBQmFHJCKvCAfQ4CYjIqGgBedpasMBB2ODgAWkO4UhhyGbwAczhyEHQ5JDkUALgAIxALkGA4dBQwMDEG7EEJwUJAmgiEwTKYzKqsA5-AFAuADTboJBwAAscAAfr9MQAOPEEuAARgAbPsjidMsA3sQUeRaCA4ABrGCUFlMMChBEhIhXPBnKAGVGbArwfE5PDkZjsYAoEFDQzGMxwSTSViKkHrfmRNweWK+LD+QL8g4KOB5SVFJg5C1wK1wABEanQLA0ahM4BgGhdcE0cDNzHwPXg3RWfRoQbdAHofSIvXGPX6XUA)

## Lists

Lists can be found throughout CSS, notably in [selectors](./selectors.md) and many properties such as [`transitionProperty`](./animations.md#transitions) (which accepts a list of properties to animate). Because they are often overloaded with different types of values, Tecton uses a list syntax based on [nested tuples](https://pursuit.purescript.org/packages/purescript-tuples/4.0.0/docs/Data.Tuple.Nested#v:(/\\)) that supports heterogeneous data. Here is how the syntax looks:

```haskell
module Example.StyleSheet where

import Data.Tuple.Nested ((/\))
import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do

  -- A selector list matching multiple HTML element types
  a /\ button /\ summary ? Rule.do

    -- A list of multiple properties to transition
    transitionProperty := color /\ backgroundColor

    -- A list of transition durations corresponding to each property
    transitionDuration := ms 150 /\ ms 75
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4YZgGcAxilpgpAgCQBKVQB4AfBo0s2HRADNTMeZQEJzlvIdbp2lACq0CnCPRzzazdGt6EX5aDTgUWTh3T28YX39Ao2cTUVkUCywAYRAYWnlBCBF0ixy8gsNklzgAERQ8FCxXCEwYLAA5GFk8GDA4AQEAegAdAyqTV3sQEXG3KZEsACVoeEi4ZdgmbqJ4sgo4AC4DuCycHAZt4j3KAF5pECY4OABaZ7gAQThZGFgrdjgoHJKMB6vISMwRABzODAaB4AKwOAACVcAFkADJwH4wYAwESUQjoLoMJ4oOAjOAAIwgeDw03Jwy+EGAII8cAA-OsVlgwA8SU8Xm9PoDunAQKYYXCEfB0LQQESOMwunA6SraChiuJmNN+U88OrNfDptx5XlCIc7vIQFB-hTKSh5ABrSFyopgHI22iPAWvD4AoFiiX6jWyLX0yDqo3FOBW2j0WTOEIQ6GqmAOkhwWWmjgEXVqkNhkQ1bz1bUiC0wqIARgArAAGBmVuAAdhrTBBEMOx1sFiscFE4gYHfLN35MS8Pj8ATwWGCoTzajgRRKMDK+UYArgi7gACJtLKYLptFb+Lod3A9HA53lSORKAfaQQvoQrneL7pd9pBifD1+D2egA)

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

## URLs

A `URL` is a pointer to a resource, such as an image, font, or custom cursor. To construct a `URL` value, simply apply the `url` function to a URL string. The following example uses the `url` function to create a reference to a background image:

```haskell
module Example.InlineStyle where

import Tecton

inlineStyle :: Declarations _
inlineStyle = backgroundImage := url "./bg.gif"
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoCSA7KASzxgGUAXAT1jgHcALGAJxgChXCMQny4AFFlAhh4ACgCqeQuQA0cUQBIAlHNEAeAHxKlHLj0QAzAzADGvUQiOnyOzum68AKk0p8ILUiaaF05lnhEmJTgUAGc4Z1d3Mi8fcl17fUlQlGMsAGEQZhMxCDwU40zsmB0EhwjrEDx2YiISCmp4AC4muAARUygUJhRyQirwgH0OAmIyKhoAXjgAIxQTAGsAcyYQPLAcYBQl5un3KDgAIiwAehmlrCXCA0P2LeI4FsNjMzhJaVZ7vDhJ1jgIlxuDyxXxYfyBP7-OAKOB5AowIpMHKQ-4wo5qdAsDRqEzgGAaQ5wTRwcHMfB1eC1MYNGjEw5qE64kTYk6Y-GHIA)

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

In some cases, measures of different types can be used interchangeably. For example, `nil` produces a value that can be used as a length, percentage, angle, or time. Similarly, values of type `Measure Length` or `Measure Percentage` can be used wherever a `Measure LengthPercentage` is expected (although the reverse is not true).

## Colors

<!-- TODO: Migrate this to another "Colors" chapter. -->

Tecton is compatible with the [`Color` type from the _colors_ package](https://pursuit.purescript.org/packages/purescript-colors/7.0.1/docs/Color#t:Color), along with the following CSS-specific color keywords:
* `transparent`
* `currentColor`

## Images

<!-- TODO: Migrate this to another "Images" chapter. -->

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

## Positions

<!-- TODO: Migrate this to another "Backgrounds" chapter. -->

Position values are used in background positioning, transforms, and gradients. A position consists of one of the following:

* `center`
* `top`
* `right`
* `bottom`
* `left`
* X/Y keyword positions, e.g. `left ~ top`
* single length-percentage value, e.g. `px 25` or `pct 50`
* X/Y length-percentage values, e.g. `pct 10 ~ px 25`