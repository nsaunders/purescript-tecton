# Syntax

## Declarations

The main building block of style sheets and inline styles alike is a declaration. A declaration consists of two parts: a property and a value. The property describes the aspect of the element you want to style, such as its font or color, and the value specifies what you want the property to be. Tecton uses the `:=` operator to set the property on the left-hand side to the value on the right. For example, here is how to set the color of the text content within an HTML element to black:

```haskell
module Example.InlineStyle where

import Color (black)
import Tecton

inlineStyle :: Declarations _
inlineStyle = color := black
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoCSA7KASzxgGUAXAT1jgHcALGAJxgChXCMQny4AFFlAhh4ACgCqeQuQA0cUQBIAlHNEAeAHxKlHLj0QAzAzADGvUQiOnyOzum68AKk0p8ILUiaaF05lnhEmJTgUAGc4Z1d3Mi8fcl17fUlQlGMsAGEQZhMxCDwU40zsmB0EhzhMqG55ACMoFBMAa1s9J2sQPHZiIhIKangALgG4ABFTeqYUckIO8IB9DgJiMioaAF44ExAqpjgBjbqGxvZgFGI94ctjMzhJaVZT87XWOAiXNw9Y3yx-QJfXuAKOB5AowIpMHL-V5AuAAIjU6BYGjUWxEGlhcE0cF+zHwPXg3WWfRoWPhAHpUTBkWTEVTYUA)

## Declaration blocks

A simple rule can consist of a single declaration. However, in many cases you will need to use [qualified-do syntax](https://jordanmartinez.github.io/purescript-jordans-reference-site/content/11-Syntax/06-Modifying-Do-Ado-Syntax-Sugar/src/13-Qualified-Do-ps.html) to combine multiple declarations into a declaration block, as in the following:

```haskell
module Example.InlineStyle where

import Color (black, white)
import Tecton
import Tecton.Rule as Rule

inlineStyle :: Declarations _
inlineStyle = Rule.do
  color := black
  backgroundColor := white
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoCSA7KASzxgGUAXAT1jgHcALGAJxgChXCMQny4AFFlAhh4ACgCqeQuQA0cUQBIAlHNEAeAHxKlHLj0QAzAzADGvUQiOnyOzum68AKk0p8ILUiaaF05lnhEmJTgUAGc4Z1d3Mi8fcl17fUlQlGMsAGEQZhMxCDwU40zsmB0EhzhMqG55ACMoFBMAazkGaRKy-UdrEDwOp268LAAlaHgwuBHYdmIiEgpqeAAuRbgAEVN6phRyQh7wgH0OAmIyKhoAXgnRrDAQVjg4ExAqpjhFy7qGxvu4Gq+AcyYIDyYEq1XedHobXYwBQxDeK0sxjMcEk0lYsPh5x+kTcHliviw-kCPweCjgeQKMCKTBypLg5LgACI1OgWBo1E8RBomXBNHBicx8LN4DMTvMaPyWQB6LkwDnStnyplAA)

## Rules

A rule consists of a prelude and either nested rules or a declaration block, joined together using the `?` operator.

> **Note**
> The `?` operator is equivalent to the pair of curly braces surrounding a rule or declaration block in CSS.

The most common type of rule is a style rule, consisting of a selector to the left of the `?` operator and a declaration block to the right, e.g.

```haskell
module Example.StyleSheet where

import Color (black, white)
import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  universal ? Rule.do
    backgroundColor := black
    color := white
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4YZgGcAxilpgpAgCQBKVQB4AfBo0s2HRADNTMeZQEJzlvIdbp2lACq0CnCPRzzazdGt6EX5aDTgUWTh3T28YX39Ao2cTUVkUCywAYRAYWnlBCBF0ixy8gsNklzgcqHY4AQAjKBR5AGspGnEYR2M3exARKpNXAZEsACVoeEi4KdgmWUJiMgo4AC51mpwcBiWieNXKAF5pEAY4OCLmADc89Kg4AH456awwc8vLxta2gHNaCAimBavV1qdmr8Ll95CA6rQNqcunhGAxgChmCINltbBYrHBROI0RiscdoTEvD4-AE8FhgqFoZc1FdihkYGV8owvnBmXAAETadD0XTaWH8XR8uB6OD0vKkciUIUUQhwfYrBVS3T87QAejFMBFOqVEqAA)

Another type of rule is a media query, whose prelude specifies the conditions under which the nested rules apply. In the following example, the style rule applies to a printed page:

```haskell
module Example.StyleSheet where

import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  media print {} ? do
    body ? Rule.do
      margin := inch 1
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4YZgGcAxilpgpAgCQBKVQB4AfBo0s2HRADNTMeZQEJzlvIdbp2lACq0CnCPRzzazdGt6EX5aDTgUWTh3T28YX39Ao2cTUVkUCywAYRAYWnlBCBF0ixy8gsNkl2j7EBEqk1dakSwAJWh4SLh22CZZQmIyCjgALhG4LJwcBn6ieKHKAF5pEAY4OGAYGRQ4dH8RSgBvAF84AH4VtfW4ACNwAnPujqwwVevr4CUAc2YRUeXfvISHAAIxMT6-UbjWwWKxwUTiBgQv6LK4xLw+PwBPBYYKhK7rNRwIolGBlfKMd5EuAAIm0exgum08nAjJpcD0cDxeVI5EoDLwhDgs0GfI5ulp2gA9Cz+EypQzdDSgA)

## Lists

Lists can be found throughout CSS, notably in [selectors](./selectors.md) and many properties such as [`transitionProperty`](./animations.md#transitions) (which accepts a list of properties to animate). Tecton's list syntax uses [the `/\` operator (nested tuples)](https://pursuit.purescript.org/packages/purescript-tuples/4.0.0/docs/Data.Tuple.Nested#v:(/\\)). Here is how the syntax looks:

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

> **Note**
> The `/\` operator is generally equivalent to `,` in CSS.

> **Note**
> If you're just getting started with Tecton, you may wonder why nested tuples are used to represent lists, rather than the more "obvious" structures like `Array` or even `List`. The reason for this relates to Tecton's highly polymorphic API, which is designed to mimic the "flexible" nature of CSS itself, whose design takes full advantage of dynamic typing. `Array` and `List` are homogenous structures, while list items in Tecton frequently have various data types.