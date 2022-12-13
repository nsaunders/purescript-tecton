-- https://www.w3.org/TR/css-overflow-3/

module Test.OverflowSpec where

import Prelude

import Tecton
  ( auto
  , clip
  , ellipsis
  , hidden
  , inherit
  , initial
  , overflow
  , overflowX
  , overflowY
  , scroll
  , textOverflow
  , unset
  , visible
  , (:=)
  , (~)
  )
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFromInline)

spec :: Spec Unit
spec = do

  let isRenderedFrom = isRenderedFromInline

  describe "Overflow Module" do

    describe "overflow-x property" do

      "overflow-x:inherit" `isRenderedFrom` (overflowX := inherit)

      "overflow-x:initial" `isRenderedFrom` (overflowX := initial)

      "overflow-x:unset" `isRenderedFrom` (overflowX := unset)

      "overflow-x:visible" `isRenderedFrom` (overflowX := visible)

      "overflow-x:hidden" `isRenderedFrom` (overflowX := hidden)

      "overflow-x:clip" `isRenderedFrom` (overflowX := clip)

      "overflow-x:scroll" `isRenderedFrom` (overflowX := scroll)

      "overflow-x:auto" `isRenderedFrom` (overflowX := auto)

    describe "overflow-y property" do

      "overflow-y:inherit" `isRenderedFrom` (overflowY := inherit)

      "overflow-y:initial" `isRenderedFrom` (overflowY := initial)

      "overflow-y:unset" `isRenderedFrom` (overflowY := unset)

      "overflow-y:visible" `isRenderedFrom` (overflowY := visible)

      "overflow-y:hidden" `isRenderedFrom` (overflowY := hidden)

      "overflow-y:clip" `isRenderedFrom` (overflowY := clip)

      "overflow-y:scroll" `isRenderedFrom` (overflowY := scroll)

      "overflow-y:auto" `isRenderedFrom` (overflowY := auto)

    describe "overflow property" do

      "overflow:inherit" `isRenderedFrom` (overflow := inherit)

      "overflow:initial" `isRenderedFrom` (overflow := initial)

      "overflow:unset" `isRenderedFrom` (overflow := unset)

      "overflow:visible" `isRenderedFrom` (overflow := visible)

      "overflow:hidden" `isRenderedFrom` (overflow := hidden)

      "overflow:clip" `isRenderedFrom` (overflow := clip)

      "overflow:scroll" `isRenderedFrom` (overflow := scroll)

      "overflow:auto" `isRenderedFrom` (overflow := auto)

      "overflow:visible hidden" `isRenderedFrom` (overflow := visible ~ hidden)

      "overflow:hidden clip" `isRenderedFrom` (overflow := hidden ~ clip)

      "overflow:clip scroll" `isRenderedFrom` (overflow := clip ~ scroll)

      "overflow:scroll auto" `isRenderedFrom` (overflow := scroll ~ auto)

      "overflow:auto visible" `isRenderedFrom` (overflow := auto ~ visible)

    describe "text-overflow property" do

      "text-overflow:inherit" `isRenderedFrom` (textOverflow := inherit)

      "text-overflow:initial" `isRenderedFrom` (textOverflow := initial)

      "text-overflow:unset" `isRenderedFrom` (textOverflow := unset)

      "text-overflow:clip" `isRenderedFrom` (textOverflow := clip)

      "text-overflow:ellipsis" `isRenderedFrom` (textOverflow := ellipsis)
