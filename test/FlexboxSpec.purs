-- https://www.w3.org/TR/css-flexbox-1/

module Test.FlexboxSpec where

import Prelude

import PSCSS (column, columnReverse, inherit, initial, nowrap, row, rowReverse, unset, wrap, wrapReverse)
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

      "flex-direction:column-reverse"
        `isRenderedFrom`
        { flexDirection: columnReverse }

    describe "flex-wrap property" do

      "flex-wrap:inherit" `isRenderedFrom` { flexWrap: inherit }

      "flex-wrap:initial" `isRenderedFrom` { flexWrap: initial }

      "flex-wrap:unset" `isRenderedFrom` { flexWrap: unset }

      "flex-wrap:nowrap" `isRenderedFrom` { flexWrap: nowrap }

      "flex-wrap:wrap" `isRenderedFrom` { flexWrap: wrap }

      "flex-wrap:wrap-reverse" `isRenderedFrom` { flexWrap: wrapReverse }
