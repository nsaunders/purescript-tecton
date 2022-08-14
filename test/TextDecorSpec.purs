-- https://www.w3.org/TR/css-text-decor-3/

module Test.TextDecorSpec where

import Prelude

import Color (rgb)
import Data.Tuple.Nested ((/\))
import PSCSS (em, inherit, initial, nil, none, px, unset, (~))
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFrom)

spec :: Spec Unit
spec =
  describe "Text Decoration Module" do

    describe "text-shadow property" do

      "text-shadow:inherit" `isRenderedFrom` { textShadow: inherit }

      "text-shadow:initial" `isRenderedFrom` { textShadow: initial }

      "text-shadow:unset" `isRenderedFrom` { textShadow: unset }

      "text-shadow:none" `isRenderedFrom` { textShadow: none }

      "text-shadow:#ffc0cb 1px 1px 2px"
        `isRenderedFrom`
        { textShadow: rgb 255 192 203 ~ px 1 ~ px 1 ~ px 2 }

      "text-shadow:#ffcc00 1px 0 10px"
        `isRenderedFrom`
        { textShadow: rgb 255 204 0 ~ px 1 ~ nil ~ px 10 }

      "text-shadow:#558abb 5px 5px"
        `isRenderedFrom`
        { textShadow: rgb 85 138 187 ~ px 5 ~ px 5 }

      "text-shadow:#ff0000 2px 5px"
        `isRenderedFrom`
        { textShadow: rgb 255 0 0 ~ px 2 ~ px 5 }

      "text-shadow:2px 4px 3px"
        `isRenderedFrom`
        { textShadow: px 2 ~ px 4 ~ px 3 }

      "text-shadow:5px 10px" `isRenderedFrom` { textShadow: px 5 ~ px 10 }

      "text-shadow:#ff0000 1px 1px 2px,#0000ff 0 0 1em,#0000ff 0 0 0.2em"
        `isRenderedFrom`
        let
          blue = rgb 0 0 255
        in
          { textShadow:
              rgb 255 0 0 ~ px 1 ~ px 1 ~ px 2
              /\ blue ~ nil ~ nil ~ em 1
              /\ blue ~ nil ~ nil ~ em 0.2
          }
