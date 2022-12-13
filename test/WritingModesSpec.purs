-- https://www.w3.org/TR/css-writing-modes-4/

module Test.WritingModesSpec where

import Prelude

import Tecton (direction, inherit, initial, ltr, rtl, unset, (:=))
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFromInline)

spec :: Spec Unit
spec = do

  let isRenderedFrom = isRenderedFromInline

  describe "Writing Modes Module" do

    describe "direction property" do

      "direction:inherit" `isRenderedFrom` (direction := inherit)

      "direction:initial" `isRenderedFrom` (direction := initial)

      "direction:unset" `isRenderedFrom` (direction := unset)

      "direction:ltr" `isRenderedFrom` (direction := ltr)

      "direction:rtl" `isRenderedFrom` (direction := rtl)
