-- https://www.w3.org/TR/css-backgrounds-3/

module Test.BackgroundsSpec where

import Prelude

import Color (rgb)
import PSCSS (currentColor, inherit, initial, transparent, unset)
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFrom)

spec :: Spec Unit
spec =
  describe "Backgrounds and Borders Module" do

    describe "background-color property" do

      "background-color:inherit" `isRenderedFrom` { backgroundColor: inherit }

      "background-color:initial" `isRenderedFrom` { backgroundColor: initial }

      "background-color:unset" `isRenderedFrom` { backgroundColor: unset }

      "background-color:#0000ff"
        `isRenderedFrom`
        { backgroundColor: rgb 0 0 255 }

      "background-color:currentColor"
        `isRenderedFrom`
        { backgroundColor: currentColor }

      "background-color:transparent"
        `isRenderedFrom`
        { backgroundColor: transparent }
