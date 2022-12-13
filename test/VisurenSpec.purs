-- https://www.w3.org/TR/CSS2/visuren.html

module Test.VisurenSpec where

import Prelude

import Tecton
  ( auto
  , both
  , clear
  , float
  , inherit
  , initial
  , left
  , none
  , right
  , unset
  , zIndex
  , (:=)
  )
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFromInline)

spec :: Spec Unit
spec = do

  let isRenderedFrom = isRenderedFromInline

  describe "Visual Formatting Model" do

    describe "float property" do

      "float:inherit" `isRenderedFrom` (float := inherit)

      "float:initial" `isRenderedFrom` (float := initial)

      "float:unset" `isRenderedFrom` (float := unset)

      "float:left" `isRenderedFrom` (float := left)

      "float:right" `isRenderedFrom` (float := right)

      "float:none" `isRenderedFrom` (float := none)

    describe "clear property" do

      "clear:inherit" `isRenderedFrom` (clear := inherit)

      "clear:initial" `isRenderedFrom` (clear := initial)

      "clear:unset" `isRenderedFrom` (clear := unset)

      "clear:none" `isRenderedFrom` (clear := none)

      "clear:left" `isRenderedFrom` (clear := left)

      "clear:right" `isRenderedFrom` (clear := right)

      "clear:both" `isRenderedFrom` (clear := both)

    describe "z-index property" do

      "z-index:inherit" `isRenderedFrom` (zIndex := inherit)

      "z-index:initial" `isRenderedFrom` (zIndex := initial)

      "z-index:unset" `isRenderedFrom` (zIndex := unset)

      "z-index:auto" `isRenderedFrom` (zIndex := auto)

      "z-index:3" `isRenderedFrom` (zIndex := 3)
