# Tecton: CSS in PureScript

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

> **Warning**
> As of this writing on May 16, 2023, Try PureScript provides an outdated package set. Until it is updated, some examples will fail to compile in that environment.

## "Hello world" example

This example serves as a good starting point for experimenting with Tecton. Simply update the `styleSheet` function with your own rules to see Tecton's CSS output.

```haskell
module Example.StyleSheet where

import Color (rgb)
import Data.Tuple.Nested ((/\))
import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  universal &. ClassName "hello-world" ? Rule.do
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
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4YZgGcAxilpgpAgCQBKVQB4AfBo0s2HRADNTMeZQEJzlvIdbp2lACq0CnCPRzzazdGt6EX5aDTgUWTh3T28YX39Ao2cTUVkUCywAYRAYWnlBCBF0ixy8gsNklzgcqHY4AVoAcwAjR2NKABEUPBQsVwhMGCwAORhZPBgwBoEAegAdAyqTV3sQEWW3NZEsACVoeEi4fdgmCaJ4sgo4AC4bmpwcBnPiK8oAXmkQBjg4IuYAG55dJQOAAMiwNSgkVkIzQ8AARGQoHUALRUdhQMAIuAAfmOBywYG+v1+VGYYDwJFun3QSDgABYAAxMn6ksjMJokSg3Wn0gBMLLZvxaKHkAGsmrQQEUwLV6ry4M0WnAmXAAKwagBswrg8hAdVoNKVrTg-IAzPy4ABGACctptDIAHLqZLJMCgCMbTLAkLqUFBOSIAJKTYBRRUFESTWi6gBWEAmzFMBBy0Zg0eNUZjutM6zwADE0MwoF7FQiADIwJAZ7FwBZwdLFHB5ZO5-M4ZgAL3girpcEt7ejAHUYJzucaAOxChjAFDMES3e62CxWOCicSz+eL95smJeHx+AJ4LDBUK6tR-YoZGBlfKMUlwS9wBHadD0XTafX8XQ4vRK2s8lIchKHfChCEbQhXhAuB-1fWZvxgT9ZjA38gA)

## API documentation

API documentation is available on [Pursuit](https://pursuit.purescript.org/packages/purescript-tecton).

## Troubleshooting

If you encounter an error that is difficult to understand, please try the following resources:
- The [test suite](https://github.com/nsaunders/purescript-tecton/tree/master/test) may offer an example of what you are trying to achieve.
- The W3C publishes the [CSS specifications](https://www.w3.org/TR/?tag=css) that guide the design of this library.

## Support

If you get stuck, help is available in the [PureScript Discord](https://purescript.org/chat) chat or on the [PureScript Discourse](https://discourse.purescript.org/) forum.

## Contributing

Contributions are welcome. Please see the [Contributing guide](https://github.com/nsaunders/purescript-tecton/blob/master/CONTRIBUTING.md) for more information.