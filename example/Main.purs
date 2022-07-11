module Example.Main where

import Prelude

import Color (hsl)
import Effect (Effect)
import Effect.Console (log)
import PSCSS (all, media, pct, pretty, px, render, universal, (?), (|*), (|>), (@=), (~=), (^=))

main :: Effect Unit
main = log $ render pretty do
  universal ?
    { color: hsl 240.0 1.0 0.5
    }
  media all { minWidth: px 600, maxWidth: px 999 } ? do
    universal # "class" ~= "foo" # "class" ^= "bar" # "class" @= "whatever" |> universal |* universal # "class" @= "foo" ?
      { opacity: 0.75
      }
    universal |> universal ?
      { width: pct 100
      , height: pct 100
      }
