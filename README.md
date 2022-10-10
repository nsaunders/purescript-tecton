# Tecton

[![CI](https://github.com/nsaunders/purescript-tecton/workflows/CI/badge.svg?branch=master)](https://github.com/nsaunders/purescript-tecton/actions?query=workflow%3ACI+branch%3Amaster)
[![Latest release](http://img.shields.io/github/release/nsaunders/purescript-tecton.svg)](https://github.com/nsaunders/purescript-tecton/releases)

Tecton is a domain-specific language for authoring CSS, embedded in PureScript. The unique capabilities of PureScript allow Tecton's strongly-typed interface to guard against a wide range of [errors](examples/TypeSafety.purs) while offering a developer experience comparable to that of using CSS directly.

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
  height: 100%;
  padding: 16px 32px;
  width: 100%;
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