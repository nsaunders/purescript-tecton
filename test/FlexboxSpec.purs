-- https://www.w3.org/TR/css-flexbox-1/

module Test.FlexboxSpec where

import Prelude

import PSCSS (column, columnReverse, inherit, initial, row, rowReverse, unset)
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFrom)

spec :: Spec Unit
spec =
  describe "Flexible Box Layout Module" do

    describe "flex-direction property" do

      "flex-direction:inherit" `isRenderedFrom` { flexDirection: inherit }

      "flex-direction:initial" `isRenderedFrom` { flexDirection: initial }

      "flex-direction:unset" `isRenderedFrom` { flexDirection: unset }

      "flex-direction:row" `isRenderedFrom` { flexDirection: row }

      "flex-direction:row-reverse"
        `isRenderedFrom`
        { flexDirection: rowReverse }

      "flex-direction:column" `isRenderedFrom` { flexDirection: column }

      "flex-direction:column-reverse"
        `isRenderedFrom`
        { flexDirection: columnReverse }
