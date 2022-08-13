-- https://www.w3.org/TR/css-writing-modes-4/

module Test.WritingModesSpec where

import Prelude

import PSCSS (inherit, initial, ltr, rtl, unset)
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFrom)

spec :: Spec Unit
spec =
  describe "Writing Modes Module" do

    describe "direction property" do

      "direction:inherit" `isRenderedFrom` { direction: inherit }

      "direction:initial" `isRenderedFrom` { direction: initial }

      "direction:unset" `isRenderedFrom` { direction: unset }

      "direction:ltr" `isRenderedFrom` { direction: ltr }

      "direction:rtl" `isRenderedFrom` { direction: rtl }
