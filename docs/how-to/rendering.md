# Rendering

Just as PureScript code must be compiled to JavaScript to run in the web browser, CSS rules written using Tecton must be rendered to plain CSS strings. Tecton provides two rendering functions depending on your use case, as well as options for pretty-printing or compact output.

## Inline styles

To render a group of [declarations](./declarations.md), use the `renderInline` function. As the name suggests, the output is appropriate for inline styles, i.e. for use in the `style` attribute of an HTML element. For example:

```haskell
module Example where

import Prelude (Unit, ($), (<>))
import Effect (Effect)
import TryPureScript (render) as TryPureScript
import Unsafe.Coerce (unsafeCoerce)

import Color (rgb)
import Tecton
import Tecton.Rule as Rule

renderedInlineStyle :: String
renderedInlineStyle =
  renderInline Rule.do
    width := px 400
    height := px 200
    backgroundColor := rgb 0 5 56
    color := rgb 232 199 148
    display := flex
    alignItems := center
    justifyContent := center
    fontFamily := sansSerif
    fontSize := px 32
    fontWeight := 700

main :: Effect Unit
main =
  TryPureScript.render
    $ unsafeCoerce
    $ "<div style=\"" <> renderedInlineStyle <> "\">Hello world!</div>"
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdZwO4AsYBOMAUMQJYYgEAucACkVBGPABQCqAdmdQDRysAJAEp+rADwA+YcPKUaiAGaKYAY1qsEytdVkV0VWgBUCATzoQiAZVUEy6DUU4sCwuCgDOcE+cswbdg5yBgpcHigqAHQAwiCEqmwQnOEqsfEwssGGcLFQVAIEAOYARnryxjognFkKRpWckQBK0PCecM2wpE4uMGAAkpxQZJz+1KZYAFwTcFbUdpyFxN2EvQNDI7Pj8AC8xHBwywRrw-AdMJFgIHv7OGRg1LhwE9tw6EhwACwADF-X+-hkQq4WjPV7vABMPz+cGKKFUAGtCgQQEkwLl8qCisU4F84ABWfEANmhqhAeQITxeWLg4IAzOC4ABGACczKZHwAHNCwGQPJgUKZKXBFLAkNCUENCpw+tQYMAvKCEpxZQRoQArCAeahkRSmWLKmDKoVKlXQxRVagAMTQZCggtB4WSVkIOrNFqsZAAXvBQW84PS3cqAOowQHAoUAdihxGAKGGT2mWhU6jgXB4MbjnDgu32Pgs1ls9mokUO0MEcCSKRgaQICTLcAAROIeQA3OBarbbAA6DYbcCkB0NPX6gxOmywA4bPckAAkYFA8jgqFAwABCcQAelbkgbQA)

## Style sheets

For a style sheet, which contains statements (e.g. [selectors](./selectors.md) and at-rules) instead of "top-level" declarations, use the `renderSheet` function. This function accepts a _configuration_ argument followed by the CSS parameter. For configuration, choose between the following options:

1. `pretty` prints the CSS output in a human-readable format.
2. `compact` optimizes the output for performance, for example removing unnecessary whitespace and using short hex strings for colors.

The following example demonstrates how to render a style sheet with each configuration. Open it in Try PureScript to compare the output.

```haskell
module Example where

import Prelude (Unit, ($), (<>))
import Effect (Effect)
import TryPureScript (render) as TryPureScript
import Unsafe.Coerce (unsafeCoerce)

import Color (rgb)
import Tecton
import Tecton.Rule as Rule

containerClass = "container" :: String

css :: CSS
css =
  universal &. containerClass ? Rule.do
    width := px 400
    height := px 200
    backgroundColor := rgb 0 5 56
    color := rgb 232 199 148
    display := flex
    alignItems := center
    justifyContent := center
    fontFamily := sansSerif
    fontSize := px 32
    fontWeight := 700

main :: Effect Unit
main =
  TryPureScript.render
    $ unsafeCoerce
    $ "<style>" <> renderSheet compact css <> "</style>" <>
      "<div class=\"" <> containerClass <> "\">Hello world!</div>" <>
      "<pre><code>\npretty:\n\n" <>
      renderSheet pretty css <>
      "\n\ncompact:\n" <>
      renderSheet compact css <>
      "</pre></code>"
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdZwO4AsYBOMAUMQJYYgEAucACkVBGPABQCqAdmdQDRysAJAEp+rADwA+YcPKUaiAGaKYAY1qsEytdVkV0VWgBUCATzoQiAZVUEy6DUU4sCwuCgDOcE+cswbdg5yBgpcHigqAHQAwiCEqmwQnOEqsfEwssGGcLFQVAIEAOYARnryxjognFkKRpWckQBK0PCecM2wpKpV1ChknITRUJ5eALxwAETdnL39hBNwAFyLcFbUdpyFXR5eyzlWVsSqO3CjxHBwSWQAboThUHAAZJFw07MDBEMjcAD87S2RMAgc4XHBkMDUXBLcboJBwAAsAAZESCLvgyIVcLRFjC4QAmZGouDFFCqADWhQIICSYFy+RxcCKxTgiLgAFZ2QA2IndPIEaGMkpwPEAZjxcAAjABOKWS+EADiJYDIHkwKFMAsUsCQRJQUAxnAAktQYMBduMEjNCESAFYQDzUMiKUyxK0zAWWk0EImKHoAMTQZCgGoZ4WSVkITp9PSsZAAXvAGbC4GLozMAOowDFYgUAdkJxGAfU4SxWWhU6jgXB4heLpxBPgs1ls9mokScLiJgkuyQiMDSBASXcm4gdplgkgWUkZMGchCs+BgtG6GFJy5O04m4gA9GOJ1PJESLlvlddXsMdqMADoTA+vHrFwYXrybm+SAASMCgeRwVCgYAAQh3U9JzgKQjxHdAiEkcRuhYSQr04KCl2oUxFkQxCDwgjt50XWhkOoVDXg3Q9QVBCYMM4Fd0DXdDOCwsiLhwggFxgJd71XStjhfUjGK3bdkJg7c4JgScgA)