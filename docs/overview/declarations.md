# Declarations

The main building block of style sheets and inline styles alike is a declaration. A declaration consists of two parts: a property and a value. The property describes the aspect of the element you want to style, such as its font or color, and the value specifies what you want the property to be. Tecton uses the `:=` operator to set the property on the left-hand side to the value on the right. For example, here is how to set the color of the text content within an HTML element to black:

```haskell
module Example.InlineStyle where

import Color (black)
import Tecton

inlineStyle :: Declarations _
inlineStyle = color := black
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoCSA7KASzxgGUAXAT1jgHcALGAJxgChXCMQny4AFFlAhh4ACgCqeQuQA0cUQBIAlHNEAeAHxKlHLj0QAzAzADGvUQiOnyOzum68AKk0p8ILUiaaF05lnhEmJTgUAGc4Z1d3Mi8fcl17fUlQlGMsAGEQZhMxCDwU40zsmB0EhzhMqG55ACMoFBMAa1s9J2sQPHZiIhIKangALgG4ABFTeqYUckIO8IB9DgJiMioaAF44ExAqpjgBjbqGxvZgFGI94ctjMzhJaVZT87XWOAiXNw9Y3yx-QJfXuAKOB5AowIpMHL-V5AuAAIjU6BYGjUWxEGlhcE0cF+zHwPXg3WWfRoWPhAHpUTBkWTEVTYUA)

## Rules

A simple rule can consist of a single declaration. More often, multiple declarations are combined to create a complex rule using [qualified-do](https://jordanmartinez.github.io/purescript-jordans-reference-site/content/11-Syntax/06-Modifying-Do-Ado-Syntax-Sugar/src/13-Qualified-Do-ps.html) syntax. In the following example, a second declaration sets the background color of an HTML element to white:

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