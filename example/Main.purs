module Example.Main where

import Prelude

import Color (hsl)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import PSCSS (all, att, class', hover, keyframes, keyframesName, nil, media, pct, pretty, px, render, table, universal, (?), (|*), (|+), (|>), (&.), (&:), (&@), (@=), (^=), (~=), (@+@), (*@))

main :: Effect Unit
main = log $ render pretty do
  media all { minWidth: px 600, maxWidth: px 999 } ? do
    keyframes (keyframesName "foo") ? do
      pct 0 @+@ pct 20 ?
        { width: 2 *@ (px 100 @+@ pct 10)
        }
      pct 80 /\ nil /\ pct 100 ?
        { width: px 100
        }
  (universal &: hover |+ universal &. "foo") /\ universal &. "bar" ?
    { color: hsl 240.0 1.0 0.5
    }
  media all { minWidth: px 600, maxWidth: px 999 } ? do
    universal &@ class' ~= "foo" &@ att "class" ^= "bar" &@ att "class" @= "whatever" |> universal |* universal &@ att "class" @= "foo" ?
      { opacity: 0.75
      }
    universal &. "lg" |> table ?
      { width: pct 100
      , height: pct 100
      }
