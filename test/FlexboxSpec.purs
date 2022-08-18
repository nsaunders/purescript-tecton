-- https://www.w3.org/TR/css-flexbox-1/

module Test.FlexboxSpec where

import Prelude

import PSCSS (auto, baseline, center, column, columnReverse, content, em, flexEnd, flexStart, inherit, initial, maxContent, minContent, nowrap, pct, px, stretch, row, rowReverse, spaceAround, spaceBetween, unset, wrap, wrapReverse)
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

    describe "order property" do

      "order:inherit" `isRenderedFrom` { order: inherit }

      "order:initial" `isRenderedFrom` { order: initial }

      "order:unset" `isRenderedFrom` { order: unset }

      "order:12" `isRenderedFrom` { order: 12 }

    describe "flex-grow property" do

      "flex-grow:inherit" `isRenderedFrom` { flexGrow: inherit }

      "flex-grow:initial" `isRenderedFrom` { flexGrow: initial }

      "flex-grow:unset" `isRenderedFrom` { flexGrow: unset }

      "flex-grow:1" `isRenderedFrom` { flexGrow: 1 }

      "flex-grow:1.5" `isRenderedFrom` { flexGrow: 1.5 }

    describe "flex-shrink property" do

      "flex-shrink:inherit" `isRenderedFrom` { flexShrink: inherit }

      "flex-shrink:initial" `isRenderedFrom` { flexShrink: initial }

      "flex-shrink:unset" `isRenderedFrom` { flexShrink: unset }

      "flex-shrink:1" `isRenderedFrom` { flexShrink: 1 }

      "flex-shrink:1.5" `isRenderedFrom` { flexShrink: 1.5 }

    describe "flex-basis property" do

      "flex-basis:inherit" `isRenderedFrom` { flexBasis: inherit }

      "flex-basis:initial" `isRenderedFrom` { flexBasis: initial }

      "flex-basis:unset" `isRenderedFrom` { flexBasis: unset }
      
      "flex-basis:10em" `isRenderedFrom` { flexBasis: em 10 }

      "flex-basis:3px" `isRenderedFrom` { flexBasis: px 3 }

      "flex-basis:50%" `isRenderedFrom` { flexBasis: pct 50 }

      "flex-basis:auto" `isRenderedFrom` { flexBasis: auto }

      "flex-basis:max-content" `isRenderedFrom` { flexBasis: maxContent }

      "flex-basis:min-content" `isRenderedFrom` { flexBasis: minContent }

      "flex-basis:content" `isRenderedFrom` { flexBasis: content }

    describe "justify-content property" do

      "justify-content:inherit" `isRenderedFrom` { justifyContent: inherit }

      "justify-content:initial" `isRenderedFrom` { justifyContent: initial }

      "justify-content:unset" `isRenderedFrom` { justifyContent: unset }

      "justify-content:flex-start"
        `isRenderedFrom`
        { justifyContent: flexStart }

      "justify-content:flex-end" `isRenderedFrom` { justifyContent: flexEnd }

      "justify-content:center" `isRenderedFrom` { justifyContent: center }

      "justify-content:space-between"
        `isRenderedFrom`
        { justifyContent: spaceBetween }

      "justify-content:space-around"
        `isRenderedFrom`
        { justifyContent: spaceAround }

    describe "align-items property" do

      "align-items:flex-start" `isRenderedFrom` { alignItems: flexStart }

      "align-items:flex-end" `isRenderedFrom` { alignItems: flexEnd }

      "align-items:center" `isRenderedFrom` { alignItems: center }

      "align-items:baseline" `isRenderedFrom` { alignItems: baseline }

      "align-items:stretch" `isRenderedFrom` { alignItems: stretch }
