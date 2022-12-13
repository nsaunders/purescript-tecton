-- https://www.w3.org/TR/CSS2/visufx.html

module Test.VisufxSpec where

import Prelude

import Tecton
  ( collapse
  , hidden
  , inherit
  , initial
  , unset
  , visibility
  , visible
  , (:=)
  )
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFromInline)

spec :: Spec Unit
spec = do

  let isRenderedFrom = isRenderedFromInline

  describe "Visual Effects" do

    describe "visibility property" do

      "visibility:inherit" `isRenderedFrom` (visibility := inherit)

      "visibility:initial" `isRenderedFrom` (visibility := initial)

      "visibility:unset" `isRenderedFrom` (visibility := unset)

      "visibility:visible" `isRenderedFrom` (visibility := visible)

      "visibility:hidden" `isRenderedFrom` (visibility := hidden)

      "visibility:collapse" `isRenderedFrom` (visibility := collapse)
