# Declarations

The smallest building block of style sheets and inline styles alike is a _declaration_. A declaration assigns a value to a CSS property. For this purpose, Tecton uses the `:=` operator, with the property appearing on the left-hand side and the value on the right. For example, `color := black` sets the color of the text content within an HTML element to black.

Multiple declarations can be combined to form a _rule_ using [qualified-do](https://jordanmartinez.github.io/purescript-jordans-reference-site/content/11-Syntax/06-Modifying-Do-Ado-Syntax-Sugar/src/13-Qualified-Do-ps.html) syntax. In the following example, a second declaration sets the background color of an HTML element to white:

```haskell
module Example where

import Prelude (Unit, ($), (<>))
import Effect (Effect)
import TryPureScript (render) as TryPureScript
import Unsafe.Coerce (unsafeCoerce)

import Color (black, white)
import Tecton
import Tecton.Rule as Rule

inlineStyle :: Declarations _
inlineStyle = Rule.do
  color := black
  backgroundColor := white

main :: Effect Unit
main =
  TryPureScript.render
    $ unsafeCoerce
    $ "<pre><code>" <> renderInline inlineStyle <> "</code></pre>"
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdZwO4AsYBOMAUMQJYYgEAucACkVBGPABQCqAdmdQDRysAJAEp+rADwA+YcPKUaiAGaKYAY1qsEytdVkV0VWgBUCATzoQiAZVUEy6DUU4sCwuCgDOcE+cswbdg5yBgpcHigqAHQAwiCEqmwQnOEqsfEwssGGcLFQVAIARlAoqgDW-Hg8GVkKRjognDXG9ZyRAErQ8J5wHbCkZJxQA-7UplgAXONwACJqxQQo1GQNXgD65IPDVqNYALw9nZFgIMRwcKogeQRw4-tFJaWncAUPAOYEIElgufm3OLhVUjAFADG5TLQqdRwLg8YjA0G7J4+CzWWz2aiRJwuJ5nQRwJIpGBpAgJHFwPEAInE6CIknEFxYkgpcCkcCxhAAkptOPABkMedsxvBWVSAPQMmB00U0yUUoA)