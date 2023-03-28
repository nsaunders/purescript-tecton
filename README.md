# Tecton

[![CI](https://github.com/nsaunders/purescript-tecton/workflows/CI/badge.svg?branch=master)](https://github.com/nsaunders/purescript-tecton/actions?query=workflow%3ACI+branch%3Amaster) [![Latest release](http://img.shields.io/github/release/nsaunders/purescript-tecton.svg)](https://github.com/nsaunders/purescript-tecton/releases) [![PureScript registry](https://img.shields.io/badge/dynamic/json?color=informational&label=registry&query=%24.tecton.version&url=https%3A%2F%2Fraw.githubusercontent.com%2Fpurescript%2Fpackage-sets%2Fmaster%2Fpackages.json)](https://github.com/purescript/registry) [![purescript-tecton on Pursuit](https://pursuit.purescript.org/packages/purescript-tecton/badge)](https://pursuit.purescript.org/packages/purescript-tecton) [![Tests](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/nsaunders/purescript-tecton/master/meta/test-count.json)](./test)

Tecton is a domain-specific language for authoring CSS, embedded in PureScript. The unique capabilities of PureScript allow Tecton's strongly-typed interface to guard against a wide range of [errors](examples/type-errors) while remaining highly flexible like vanilla CSS.

[![Live Demo](https://shields.io/badge/-Live%20Demo-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAWQIYEsB2cDuALGAnGAKEJWAAcQ8AXOABQKgjCJPMpoFEAzLmAYxoAKbrwEBKVhWpwAEkiggA5jAxIAzrMntZ8pSoB0MgCoIAMnHWyZW6XIXK0hk6f30QZfFRQwNlmbRsaOz1HADUAERBgfXCAeQR9AGUqPHRFACUVZgI8OEECNGyxCw1gh0C4IzwAT1oIAkS%2BVLIhAqKSypqKgFU0NSRefQBhEHw%2BeEEIPoGYEbGYCQqRhVz8xQAjCVIpGnCkKiR9IwgyWH0AOR8qGDA8wQB6AB0xRe3tI34qEAxBeRRFNAASWuwDUABoLGhSPsUN9wvUYd8IUgocBEUDrnh0SMplRkaj0eckMAYPjoV5vkZSGkAGJTASwtAQ9ZIPgAa0UeBAUzAy0ozMo2XSSDAKAg4Lg60F%2BGS1VgAoAHolsCKQJgIeM0JiNSAVhDmIp9Sg1KckNUIVxYAqLSA%2BOKbVqacSUFBzXAuN8qIkUAAvUnuz0AdRg-2weLgiiQZAhuFD4ewIAAbvgIeguOgUNdU2goOgYDSrRCAFbirxcaojLUqcNsmDVLhYkkS2v1xs%2BIkkiG5tAwGQhxRhrt5pB4CFovCKdAQqFQafff3cqjdmCy%2BVwMgi0VoQ3rgQQsgEKhUN1ka1wNoy3AwcNcg5ZuD9PqJfAoLgQtT8CHXBVUcL8SjoqYeZflifQengwAQlMKDJng-SzlgKBgFQ2AQoIABkABcYhoQA-DheSYQAvARggAH4vBUHwCN8hi6A4dzofolFvNI1FfI46TQPAlhcbAxBougcCYZhcAiJ8cC9JmhCCRgRGdNU%2BgXrkAAkcBgLacB8GoGhhsAUCEHAWC4AQhlGVpnqoD2uTyTIwxQOoagdvAABEfCWXmeAuWZRnrBAR7fHAtn2Y5zlwC5fkBWg3nmQ%2BZBCcFQwOTpYUuSa6AxeZUxJaFxITHZOUpXlcAKsU8kKj5cCVelaAAIIEhSclwC2DZ5U5xVpfF0XELF2kaPJGmVZVRnQbB8FwExFlalZ%2BBwLhcB8TA%2BiDbFsWYEhKHCfJp5wAATAADPtw3mbGA40MR64Knth3Hb5rIclyPJ8rkF0TuscAAIwAKyfQAHHtADMt3qcaprVFt7pWsDfwAsCMCghDmqYsDJZqGWFaetWiPVvgx3HS1bYaDV9Xkoyc3qSAwNkAIcD7eTi3LZTq2rSkKJqOBwAQ7e%2BzwDOVM0x9h309xjPA0ZrNgZQnOvSAd4TAacAAwAbPtEjM8do34ONk01cLZwrczkKk98YUXcTDWMtDFtwgijUQx%2BfCfVbxsYvg2LclqENphm1zO2ijVUsAtL0nbF3LiOwMa1CY3yBN%2BiSv5HF60tBvM4uy6rvAF1oPOwPfr%2B-5Yo1QE9hDOc9sDUp4Nkmdl7nhuSiASoqhpmBcxscBfR9ndK3ASu7XAZFwDOg-Dy6o8j0PO0fZX92ch7vK6pQ7fvQDdMAwALHAG9HQ37krCvn0AJxbwdAN7R9u2V9KeDCqK4oQztSvAx6jrOq6EMuQAtFGpwwF-ahqho3hi5OATxwoACFuxsmQHwRIQCQQ0k9KA8BLlnyKFGJJQEKDHjhXOLLEAcBEhsxweFPsUBkxeD4EgUhLlaqpHkKQx8ahnypC4C-T03o-SPyuh9DeHCtTBjjBDL6N0G7Lj7MIi6M8G6ihNA5cGF10DLgLDACqDcYauwRhdJGuMG6RjIDwuAv1gbjknBgbOLoqabjSEYreU9eFXwblHGCWtY6TUiknLCcAEywWTqLBuLJ2TzyekvF68k3rbwHhvAA7J3IGgSm7KlVG3V6HdL7-Q%2BgDOJfD-pD0nmPKAE9x4OKdi4mORSPGJ0Ct4j0doNDzQZqnVaVchQijFBoSxBlEnNxSYfGJmSvq912l9H6%2BSSmFOKUU0pV9Kp6SKURY6Mg7KikTMDAA2v4fQfBkpqAAPpTQOJ5AAuhsqw%2BhPHfDFnAdZ5z9gpDyHZWqR48CpW%2BDslA7IXLFDUi5FCxp9DpjglQDgsASRaiGNgF0YBtm7KAmjfQm5BAAHJQEAB4AB8cBsq7LCrrTF4VkViAANwfioIHGAi5BCCDKli-5ahAUoGBaC%2BG1ZIXQthY5eFVAlLwyTDAFF6KsU4tyiSOKQkCUuSJRCURh0xCZQbhCLZOzHIHMuWga5pyG5GVucs-QMAwVY0EHZFlwBUpqETIob51ydV3JeY8-QzyUipXWshbA3zwofV%2Bgq7VSq7L3NWE8l5qVTphg9S5L1PrFV2oecax1waOqJm8JgCBTdw10zprtU%2BG8o2Gy1dq21eqDWss9nG01qUNwoWtQW2Kur-X2rjU615HUwDhoQJfbeoQPqmH%2Bl9KAW8N6hCVnwAG%2BgAbdzpsM-QSsj59znXTD6%2Bh9ofS-voEZn19BHxiauuJu19C-SgIujevcj1KyGB9I%2B%2Bgvpb2%2Bku8%2BB1Pqjq%2Bruhdu0%2BD7S-hvfQG9dpfyfb9L%2BAGgM%2BmAAujefA-1jonV-JWX890zpgzB2mX9F3LrXV9FDm6Yn6G3Xu36pgvprq3thmJG8hhfuPcYrD29Pr7W-efG9b66YUYHv%2B4x7HfqJgBv2z9vGN5cZ9Lm6NeqA0OqbaldMUAoDhrtHgAoVBnpCbzTa8y6z80FvU4bP1%2Bh87hXgWgPgSmjKadiqcyqGlHZ9UeXZYwZg4AGrKlHfovA5h4HGHAFSwMXJorRnKGAGKhXniyJeGA151yHmPFpHScBJVovuL52AAXrkErKAYZScAkLVkzODA1hAgA)

## Quick example

### Input
```purescript
body ? Rule.do
  width := pct 100
  height := pct 100
  padding := px 16 ~ px 32

media screen { minWidth: px 768 } ?
  body ?
    padding := pct 5 ~ pct 10
```

### Output
```css
body {
  width: 100%;
  height: 100%;
  padding: 16px 32px;
}
@media screen and (min-width: 768px) {
  body {
    padding: 5% 10%;
  }
}
```

Many more examples are located in the [examples](./examples) and [test](./test)
subdirectories.

## Installation

The preferred installation method is [Spago](https://github.com/purescript/spago).

```sh
spago install tecton
```

## Related

* [purescript-tecton-halogen](https://github.com/nsaunders/purescript-tecton-halogen)
* [purescript-tecton-halogen-starter](https://github.com/nsaunders/purescript-tecton-halogen-starter)
