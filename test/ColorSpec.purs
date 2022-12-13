module Test.ColorSpec where

import Prelude

import Color (hsl)
import Tecton
  ( color
  , currentColor
  , inherit
  , initial
  , opacity
  , transparent
  , unset
  , (:=)
  )
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFromInline)

spec :: Spec Unit
spec = do

  let isRenderedFrom = isRenderedFromInline

  describe "Color Module" do

    describe "color property" do

      "color:inherit" `isRenderedFrom` (color := inherit)

      "color:initial" `isRenderedFrom` (color := initial)

      "color:unset" `isRenderedFrom` (color := unset)

      "color:transparent" `isRenderedFrom` (color := transparent)

      "color:currentColor" `isRenderedFrom` (color := currentColor)

      "color:#0000ff" `isRenderedFrom` (color := hsl 240.0 1.0 0.5)

    describe "opacity property" do

      "opacity:inherit" `isRenderedFrom` (opacity := inherit)

      "opacity:initial" `isRenderedFrom` (opacity := initial)

      "opacity:unset" `isRenderedFrom` (opacity := unset)

      "opacity:0" `isRenderedFrom` (opacity := 0)

      "opacity:0.5" `isRenderedFrom` (opacity := 0.5)
