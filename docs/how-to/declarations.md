# Declarations

The smallest building block of style sheets and inline styles alike is a _declaration_. A declaration assigns a value to a CSS property. For this purpose, Tecton uses the `:=` operator, with the property appearing on the left-hand side and the value on the right. For example, `color := black` sets the color of the text content within an HTML element to black.

Multiple declarations can be combined to form a _rule_ using [qualified-do](https://jordanmartinez.github.io/purescript-jordans-reference-site/content/11-Syntax/06-Modifying-Do-Ado-Syntax-Sugar/src/13-Qualified-Do-ps.html) syntax. In the following example, a second declaration sets the background color of an HTML element to white:

```haskell
module Styles where

import Color (black, white)
import Tecton (backgroundColor, color, (:=))
import Tecton.Rule as Rule

myInlineStyle = Rule.do
  color := black
  backgroundColor := white
```