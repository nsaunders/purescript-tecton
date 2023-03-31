# Getting started with Tecton

Tecton is a domain-specific language for authoring CSS using [PureScript](https://purescript.org/). At a basic level, it could be compared to CSS preprocessors such as [Sass](http://sass-lang.com/) and [LESS](http://lesscss.org/). However, where these preprocessors aim to add expressivity to CSS, Tecton offers the full expressive power of its host language along with a high degree of type safety. It also unlocks a number of secondary benefits, such as reuse of existing PureScript knowledge and seamless colocation with related markup. If you're ready to write masterful CSS with Tecton, let's get started.

## Installation

### Local installation

The preferred installation method is [Spago](https://github.com/purescript/spago):

```bash
spago install tecton
```

### Try PureScript

Alternatively, to evaluate Tecton without installing anything locally, you can use [Try PureScript](https://try.purescript.org). Throughout the documentation, look for buttons like this one to launch the corresponding code example in the Try PureScript app:

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](javascript:alert("Normally%2C%20you%27ll%20be%20redirected%20to%20Try%20PureScript.%20For%20now%2C%20let%27s%20continue%20getting%20started%20with%20Tecton."))

You can give this a try now with the following example.

## "Hello world" example

This example serves as a good starting point for experimenting with Tecton. Simply update the `css` function with your own rules to see Tecton's CSS output.

```haskell
module Example where

import Prelude (Unit, ($), (<>))
import Effect (Effect)
import TryPureScript (render) as TryPureScript
import Unsafe.Coerce (unsafeCoerce)

import Color (rgb)
import Data.Tuple.Nested ((/\))
import Tecton
import Tecton.Rule as Rule

css :: CSS
css = do
  universal &. "hello-world" ? Rule.do
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

main :: Effect Unit
main =
  TryPureScript.render
    $ unsafeCoerce
    $  "<pre><code>" <> renderSheet pretty css <> "</code></pre>"
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdZwO4AsYBOMAUMQJYYgEAucACkVBGPABQCqAdmdQDRysAJAEp+rADwA+YcPKUaiAGaKYAY1qsEytdVkV0VWgBUCATzoQiAZVUEy6DUU4sCwuCgDOcE+cswbdg5yBgpcHigqAHQAwiCEqmwQnOEqsfEwssGGcLFQVAIEAOYARnrytAAiKNQokUYQmDCRAHIwHtQwYAKsAPQAOjJZCkY6IJxDxqOckQBK0PCecHOwpKoeXgBcGzlWVsRrXgC8cGAgxHBwSWQAboThUHAAZJFwAET4UHkAtNhUUGCvOAAfiW80ip3OFxwZDA1FwcA2x3QSDgABYAAzoyEXfBkQq4WiIuDIuAAJkx2LgxRQqgA1oUCCAkmBcvkiUVinB0XAAKy8gBslNUIDyBARxw5ZIAzKS4ABGACcCvlqIAHJSwGQPJgUKZxXBFLAkJSUFA8ZwAJIdYCbY4JTgdAiUgBWEHaZEUpliDpgDv19sdlMUY2oADE0GQoHqia8ADIwJC+gFwfpwcLJKyED1BkNWMgAL3gRJJMpzDoA6jA8QT9QB2CnEYAoMicBHbLQqdRwLg8RvN1uHSE+CzWWz2aiRJwuSmCS7JCIwNIEBIzi6vcToIiScTCliSQFSOBTwhWfAwWib8-UPUHOCH9c9Xcwbc9S-7oA)

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