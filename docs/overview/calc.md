# Calc expressions

Calc expressions allow you to perform calculations within CSS property values, supporting a wide range of use cases such as responsive design, grid systems, and fluid typography.

Several calc operators are supported:
* `@+@` adds two operands, e.g. `pct 90 @+@ px 16`.
* `@-@` subtracts the second operand from the first, e.g. `pct 100 - px 32`.
* `@*` multiplies the first operand by the second, e.g. `px 16 @* 2`.
* `*@` multiplies the first operand by the second, e.g. `2 *@ px 16`.
* `@/` divides the first operand by the second, e.g. `pct 100 @/ 6`.

Notice that each operator includes one or more `@` symbols. An operand that is adjacent to this symbol is a [measure](./values.md#measures); otherwise, it is a number or integer.

Here is an example of how the `@-@` and `@*` operators can be used to make a `div` element span the width of the viewport, but leaving a 32-pixel "margin" on each side:

```haskell
module Example.StyleSheet where

import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  body ? margin := nil
  div ? Rule.do
    margin := nil ~ auto -- horizontal centering
    width := vw 100 @-@ px 32 @* 2
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4YZgGcAxilpgpAgCQBKVQB4AfBo0s2HRADNTMeZQEJzlvIdbp2lACq0CnCPRzzazdGt6EX5aDTgUWTh3T28YX39Ao2cTUVkUCywAYRAYWnlBCBF0ixy8gsNkl2j7EBEqk1dakSwAJWh4SLh22CZZQmIyCjgALhG4LJwcBn6ieKHKAF5pEAY4OAAjcAI4AH44YCUAc2YRUeWxKDXpZgA3Pe6OrDBV9fXD2hOzkYvmKDgAH4RCB4EBwAC04LgJHYzAAXnU8Ch-gURHg8qcjtd1lRmGA8CRznBblQ4ABGAAMFLgAAFwTS4OgkHAAMwAJlpACo4GymIdTqNxrYLFY4KJxAx+WdFtcYl4fH4AngsMFQti4Go4EUSjAyvlGG8NesAETadD0XTaeTgGC6Y1wPRwVV5UjkSjmiiEOCzQZuh26OCmgD01v4lqDHrtQA)