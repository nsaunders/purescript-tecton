{-

This example fails to compile because multiple `<auto-repeat>` values (e.g.
`repeat(auto-fit, 100px)` or `repeat(auto-fill, 10%)`) cannot appear within the
same track list.

See https://www.w3.org/TR/css-grid-1/#typedef-auto-track-list for more
information.

-}

module TypeError.GridTemplateColumnsMultipleAutoRepeat where

import Data.Tuple.Nested ((/\))
import Tecton
  ( CSS
  , autoFill
  , autoFit
  , gridTemplateColumns
  , pct
  , px
  , repeat
  , universal
  , (:=)
  , (?)
  )
import Tecton.Rule as Rule

css :: CSS
css =
  universal ? Rule.do
    gridTemplateColumns := repeat autoFit (px 100) /\ repeat autoFill (pct 10)
