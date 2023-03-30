# Getting started with Tecton

Tecton is a domain-specific language for authoring CSS using [PureScript](https://purescript.org/). At a basic level, it could be compared to CSS preprocessors such as [Sass](http://sass-lang.com/) and [LESS](http://lesscss.org/). However, where these preprocessors aim to add expressivity to CSS, Tecton offers the full expressive power of its host language along with a high degree of type safety. It also unlocks a number of secondary benefits, such as reuse of existing PureScript knowledge and seamless colocation with related markup. If you're ready to write masterful CSS with Tecton, let's get started.

## Installation

### Try PureScript

To evaluate Tecton without installing anything locally, you can use [Try PureScript](https://try.purescript.org/). Click the button below the ["Hello world" example](#hello-world-example) to launch it in the Try PureScript app.

### Local installation

The preferred installation method is [Spago](https://github.com/purescript/spago):

```bash
spago install tecton tecton-halogen
```

> **Note**
> If you plan to use a UI framework other than [Halogen](https://github.com/purescript-halogen/purescript-halogen), the [`tecton-halogen`](https://github.com/nsaunders/purescript-tecton-halogen) package is not needed. However, the ["Hello world" example](#hello-world) uses Halogen for HTML templating.

## "Hello world" example

This example serves as a good starting point for experimenting with Tecton. Simply update the `css` and `html` functions with your own content. Note that, while Tecton is framework-independent, Halogen is used for HTML templating; [this guide](https://purescript-halogen.github.io/purescript-halogen/guide/01-Rendering-Halogen-HTML.html) can help if needed.

```haskell
module Main where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.DOM.StringRenderer (render) as Halogen
import TryPureScript (render) as Try
import Unsafe.Coerce (unsafeCoerce)

import Color (rgb)
import Data.Tuple.Nested ((/\))

import Tecton (alignItems, backgroundColor, center, color, display, flex, fontFamily, fontSize, fontWeight, height, justifyContent, pretty, px, renderSheet, sansSerif, universal, width, (?), (:=))
import Tecton.Halogen ((&.))
import Tecton.Rule as Rule

main :: Effect Unit
main = Try.render $ doc css html
  where
    container = H.ClassName "container"

    css = do
    
      universal &. container ? Rule.do
        width := px 400
        height := px 200
        backgroundColor := rgb 0 5 56
        color := rgb 232 199 148
        display := flex
        alignItems := center
        justifyContent := center
        fontFamily := "Lexend" /\ sansSerif
        fontSize := px 32
        fontWeight := 700

    html =
      HH.div
        [HP.class_ container]
        [HH.text "Hello world!"]

    doc css' (HH.HTML el) =
      unsafeCoerce $
        "<style>@import url('https://fonts.googleapis.com/css2?family=Lexend&display=swap');" <> renderSheet pretty css' <> "</style>"
          <> Halogen.render identity el
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAWQIYEsB2cDuALGAnGAKEJWAAcQ8AXOABQKgjCJPMpoFEAzLmAYxoAKbrwEBKVhWpwAEkiggA5jAxIAzrMntZ8pSoB0MgCoIAMnHWyZW6XIXK0hk6f30QZfFRQwNlmbRsaOz1HADUAERBgfXCAeQR9AGUqPHRFACUVZgI8OEECNGyxCw1gh0C4IzwAT1oIAkS%2BVLIhAqKSypqKgFU0NSRefQBhEHw%2BeEEIPoGYEbGYCQqRhVz8xQAjCVIpGnCkKiR9IwgyWH0AOR8qGDA8wQB6AB0xRe3tI34qEAxBeRRFNAASWuwDUABo4OskHwANaKPAgKZgZaUCHjNDXPBokArCFgFBqU5IaoQriwAAepO%2BVAAYkhgCgoCS4FxqYkUAAvGBUjEAdRg-2wVAhuEFwrgACsIGovFxqiMMSpxWQCFQqMyyJS4G18IlcDBxf0%2Bol8CguBCpigAG74fpQCGYFBgKjYCGCAD8YjdAC4ALwvCofATfQy6Bx3ABk%2BgDb2kQa%2BjnS0HgliTsGIwFQGG93rgIk%2BcF6KCohEz6Dgvs61X0OtyABI4GAQHw4Hw1BohcAoIQ4FhcAQe73W9Ss-gK7JhlB1GpzvT4AAiPgj9D4efEIet9vjpuD3u7oeWm14O1wKPDjGj3LuuBpmD6HcbjeO53YOB%2BuCauAAFgADD-9xuoqKEKb6Vp%2BABMf4AUOUKwvCiKFCiuTvngGxwD%2BcAAKxYQAbNBvZLisoHamh4EAMzgXAACMACcNHUV%2BAAc%2BGNgSRLVMRZIwOSLF-ACwIwKCxHopiLFSjKZrytSSrCUq%2BAsayGJ0gyTLEfOpjcVk85wE8cBGmoJqpFwClspy8Dvp%2BFEmXyArATQ74AOxQfunZQBW0EyDI97WixADa-j6HwU7tgA%2BueBwrngAC6fmefo1zkjQ84yDAUAKFglBQGAACE84xfuTYtm2agAOR5HFxhmHAqXFL60FTP0vBzHg4xwHWLHzgAPDK1SwAAfAAArGND1FAgglUKVBkGo3r3PcilUGo%2BiKCASiwEgZAEoFUT3MV4Hulw9KMtUvoaeSWQRvihJTidaiYBtJViAA3NpnV9dqWS6vqNAqga6qbqVcBvXAXX3D1-Vro%2Bj7A2UBi1nATpKsWHGpUAA)

## API documentation

API documentation is available on [Pursuit](https://pursuit.purescript.org/packages/purescript-tecton).

## Tutorial

TODO

## Troubleshooting

If you encounter an error that is difficult to understand, please try the following resources:
- The [test suite](https://github.com/nsaunders/purescript-tecton/tree/master/test) may offer an example of what you are trying to achieve.
- The W3C publishes the [CSS specifications](https://www.w3.org/TR/?tag=css) that guide the design of this library.

## Support

If you get stuck, help is available in the [PureScript Discord](https://purescript.org/chat) chat or on the [PureScript Discourse](https://discourse.purescript.org/) forum.

## Contributing

Contributions are welcome. Please see the [Contributing guide](https://discourse.purescript.org/) for more information.