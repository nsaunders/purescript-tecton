-- https://www.w3.org/TR/css-text-decor-3/

module Test.TextDecorSpec where

import Prelude

import Color (rgb)
import Data.Tuple.Nested ((/\))
import Tecton
  ( blink
  , dashed
  , dotted
  , double
  , em
  , inherit
  , initial
  , lineThrough
  , nil
  , none
  , overline
  , px
  , solid
  , textDecorationColor
  , textDecorationLine
  , textDecorationStyle
  , textShadow
  , underline
  , unset
  , wavy
  , (:=)
  , (~)
  )
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFromInline)

spec :: Spec Unit
spec = do

  let isRenderedFrom = isRenderedFromInline

  describe "Text Decoration Module" do

    describe "text-decoration-line" do

      "text-decoration-line:inherit"
        `isRenderedFrom`
          (textDecorationLine := inherit)

      "text-decoration-line:initial"
        `isRenderedFrom`
          (textDecorationLine := initial)

      "text-decoration-line:unset"
        `isRenderedFrom`
          (textDecorationLine := unset)

      "text-decoration-line:none"
        `isRenderedFrom`
          (textDecorationLine := none)

      "text-decoration-line:underline overline line-through blink"
        `isRenderedFrom`
          (textDecorationLine := underline ~ overline ~ lineThrough ~ blink)

      "text-decoration-line:overline line-through blink"
        `isRenderedFrom`
          (textDecorationLine := overline ~ lineThrough ~ blink)

      "text-decoration-line:underline line-through blink"
        `isRenderedFrom`
          (textDecorationLine := underline ~ lineThrough ~ blink)

      "text-decoration-line:underline overline blink"
        `isRenderedFrom`
          (textDecorationLine := underline ~ overline ~ blink)

      "text-decoration-line:underline overline line-through"
        `isRenderedFrom`
          (textDecorationLine := underline ~ overline ~ lineThrough)

      "text-decoration-line:line-through blink"
        `isRenderedFrom`
          (textDecorationLine := lineThrough ~ blink)

      "text-decoration-line:underline blink"
        `isRenderedFrom`
          (textDecorationLine := underline ~ blink)

      "text-decoration-line:underline overline"
        `isRenderedFrom`
          (textDecorationLine := underline ~ overline)

      "text-decoration-line:underline line-through"
        `isRenderedFrom`
          (textDecorationLine := underline ~ lineThrough)

      "text-decoration-line:overline blink"
        `isRenderedFrom`
          (textDecorationLine := overline ~ blink)

      "text-decoration-line:underline"
        `isRenderedFrom`
          (textDecorationLine := underline)

      "text-decoration-line:overline"
        `isRenderedFrom`
          (textDecorationLine := overline)

      "text-decoration-line:line-through"
        `isRenderedFrom`
          (textDecorationLine := lineThrough)

      "text-decoration-line:blink"
        `isRenderedFrom`
          (textDecorationLine := blink)

    describe "text-decoration-style" do

      "text-decoration-style:inherit"
        `isRenderedFrom`
          (textDecorationStyle := inherit)

      "text-decoration-style:initial"
        `isRenderedFrom`
          (textDecorationStyle := initial)

      "text-decoration-style:unset"
        `isRenderedFrom`
          (textDecorationStyle := unset)

      "text-decoration-style:solid"
        `isRenderedFrom`
          (textDecorationStyle := solid)

      "text-decoration-style:double"
        `isRenderedFrom`
          (textDecorationStyle := double)

      "text-decoration-style:dotted"
        `isRenderedFrom`
          (textDecorationStyle := dotted)

      "text-decoration-style:dashed"
        `isRenderedFrom`
          (textDecorationStyle := dashed)

      "text-decoration-style:wavy"
        `isRenderedFrom`
          (textDecorationStyle := wavy)

    describe "text-decoration-color property" do

      "text-decoration-color:inherit"
        `isRenderedFrom`
          (textDecorationColor := inherit)

      "text-decoration-color:initial"
        `isRenderedFrom`
          (textDecorationColor := initial)

      "text-decoration-color:unset"
        `isRenderedFrom`
          (textDecorationColor := unset)

      "text-decoration-color:#ff0000"
        `isRenderedFrom`
          (textDecorationColor := rgb 255 0 0)

    describe "text-shadow property" do

      "text-shadow:inherit" `isRenderedFrom` (textShadow := inherit)

      "text-shadow:initial" `isRenderedFrom` (textShadow := initial)

      "text-shadow:unset" `isRenderedFrom` (textShadow := unset)

      "text-shadow:none" `isRenderedFrom` (textShadow := none)

      "text-shadow:#ffc0cb 1px 1px 2px"
        `isRenderedFrom`
          (textShadow := rgb 255 192 203 ~ px 1 ~ px 1 ~ px 2)

      "text-shadow:#ffcc00 1px 0 10px"
        `isRenderedFrom`
          (textShadow := rgb 255 204 0 ~ px 1 ~ nil ~ px 10)

      "text-shadow:#558abb 5px 5px"
        `isRenderedFrom`
          (textShadow := rgb 85 138 187 ~ px 5 ~ px 5)

      "text-shadow:#ff0000 2px 5px"
        `isRenderedFrom`
          (textShadow := rgb 255 0 0 ~ px 2 ~ px 5)

      "text-shadow:2px 4px 3px"
        `isRenderedFrom`
          (textShadow := px 2 ~ px 4 ~ px 3)

      "text-shadow:5px 10px" `isRenderedFrom` (textShadow := px 5 ~ px 10)

      "text-shadow:#ff0000 1px 1px 2px,#0000ff 0 0 1em,#0000ff 0 0 0.2em"
        `isRenderedFrom`
          let
            blue = rgb 0 0 255
          in
            textShadow :=
              (rgb 255 0 0 ~ px 1 ~ px 1 ~ px 2)
              /\ blue ~ nil ~ nil ~ em 1
              /\ blue ~ nil ~ nil ~ em 0.2
