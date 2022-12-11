{-

This example fails to compile because the gradient `<color-stop-list>` syntax
does not allow two consecutive transition hints.

See https://www.w3.org/TR/css-images-3/#typedef-color-stop-list for more
information.

-}

module TypeError.GradientConsecutiveTransitionHints where

import Prelude

import Color (black, white)
import Data.Tuple.Nested ((/\))
import Tecton
  ( CSS
  , backgroundImage
  , deg
  , linearGradient
  , pct
  , universal
  , (:=)
  , (?)
  )
import Tecton.Rule as Rule

css :: CSS
css =
  universal ? Rule.do
    backgroundImage :=
      linearGradient (deg 180) $ black /\ pct 40 /\ pct 50 /\ white
