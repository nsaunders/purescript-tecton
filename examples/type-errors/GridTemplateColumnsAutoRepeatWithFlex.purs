{-

This example fails to compile because `<flex>` values (e.g. `1fr`) cannot appear
in the same track list as `<auto-repeat>` values (e.g.
`repeat(auto-fill, 100px 10%)`).

See https://www.w3.org/TR/css-grid-1/#typedef-auto-track-list for more
information.

-}

module TypeError.GridTemplateColumnsAutoRepeatWithFlex where

import Data.Tuple.Nested ((/\))
import Tecton (CSS, autoFill, fr, gridTemplateColumns, pct, px, repeat, universal, (?), (:=))

css :: CSS
css =
  universal ?
    gridTemplateColumns := fr 1 /\ repeat autoFill (px 100 /\ pct 10)
