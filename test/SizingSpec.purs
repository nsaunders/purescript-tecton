-- https://www.w3.org/TR/css-sizing-3/

module Test.SizingSpec where

import Prelude

import Tecton
  ( auto
  , borderBox
  , boxSizing
  , contentBox
  , fitContent
  , height
  , inherit
  , initial
  , maxContent
  , maxHeight
  , maxWidth
  , minContent
  , minHeight
  , minWidth
  , none
  , pct
  , px
  , unset
  , width
  , (:=)
  , (@+@)
  )
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFromInline)

spec :: Spec Unit
spec = do

  let isRenderedFrom = isRenderedFromInline

  describe "Box Sizing Module" do

    describe "width property" do

      "width:inherit" `isRenderedFrom` (width := inherit)

      "width:initial" `isRenderedFrom` (width := initial)

      "width:unset" `isRenderedFrom` (width := unset)

      "width:auto" `isRenderedFrom` (width := auto)

      "width:100px" `isRenderedFrom` (width := px 100)

      "width:50%" `isRenderedFrom` (width := pct 50)

      "width:calc(100px + 50%)" `isRenderedFrom` (width := px 100 @+@ pct 50)

      "width:min-content" `isRenderedFrom` (width := minContent)

      "width:max-content" `isRenderedFrom` (width := maxContent)

      "width:fit-content(100px)" `isRenderedFrom` (width := fitContent (px 100))

      "width:fit-content(50%)" `isRenderedFrom` (width := fitContent (pct 50))

      "width:fit-content(calc(100px + 50%))"
        `isRenderedFrom`
          (width := fitContent (px 100 @+@ pct 50))

    describe "height property" do

      "height:inherit" `isRenderedFrom` (height := inherit)

      "height:initial" `isRenderedFrom` (height := initial)

      "height:unset" `isRenderedFrom` (height := unset)

      "height:auto" `isRenderedFrom` (height := auto)

      "height:100px" `isRenderedFrom` (height := px 100)

      "height:50%" `isRenderedFrom` (height := pct 50)

      "height:calc(100px + 50%)" `isRenderedFrom` (height := px 100 @+@ pct 50)

      "height:min-content" `isRenderedFrom` (height := minContent)

      "height:max-content" `isRenderedFrom` (height := maxContent)

      "height:fit-content(100px)" `isRenderedFrom`
        (height := fitContent (px 100))

      "height:fit-content(50%)" `isRenderedFrom` (height := fitContent (pct 50))

      "height:fit-content(calc(100px + 50%))"
        `isRenderedFrom`
          (height := fitContent (px 100 @+@ pct 50))

    describe "min-width property" do

      "min-width:inherit" `isRenderedFrom` (minWidth := inherit)

      "min-width:initial" `isRenderedFrom` (minWidth := initial)

      "min-width:unset" `isRenderedFrom` (minWidth := unset)

      "min-width:auto" `isRenderedFrom` (minWidth := auto)

      "min-width:100px" `isRenderedFrom` (minWidth := px 100)

      "min-width:50%" `isRenderedFrom` (minWidth := pct 50)

      "min-width:calc(100px + 50%)"
        `isRenderedFrom`
          (minWidth := px 100 @+@ pct 50)

      "min-width:min-content" `isRenderedFrom` (minWidth := minContent)

      "min-width:max-content" `isRenderedFrom` (minWidth := maxContent)

      "min-width:fit-content(100px)"
        `isRenderedFrom`
          (minWidth := fitContent (px 100))

      "min-width:fit-content(50%)"
        `isRenderedFrom`
          (minWidth := fitContent (pct 50))

      "min-width:fit-content(calc(100px + 50%))"
        `isRenderedFrom`
          (minWidth := fitContent (px 100 @+@ pct 50))

    describe "min-height property" do

      "min-height:inherit" `isRenderedFrom` (minHeight := inherit)

      "min-height:initial" `isRenderedFrom` (minHeight := initial)

      "min-height:unset" `isRenderedFrom` (minHeight := unset)

      "min-height:auto" `isRenderedFrom` (minHeight := auto)

      "min-height:100px" `isRenderedFrom` (minHeight := px 100)

      "min-height:50%" `isRenderedFrom` (minHeight := pct 50)

      "min-height:calc(100px + 50%)"
        `isRenderedFrom`
          (minHeight := px 100 @+@ pct 50)

      "min-height:min-content" `isRenderedFrom` (minHeight := minContent)

      "min-height:max-content" `isRenderedFrom` (minHeight := maxContent)

      "min-height:fit-content(100px)"
        `isRenderedFrom`
          (minHeight := fitContent (px 100))

      "min-height:fit-content(50%)"
        `isRenderedFrom`
          (minHeight := fitContent (pct 50))

      "min-height:fit-content(calc(100px + 50%))"
        `isRenderedFrom`
          (minHeight := fitContent (px 100 @+@ pct 50))

    describe "max-width property" do

      "max-width:inherit" `isRenderedFrom` (maxWidth := inherit)

      "max-width:initial" `isRenderedFrom` (maxWidth := initial)

      "max-width:unset" `isRenderedFrom` (maxWidth := unset)

      "max-width:none" `isRenderedFrom` (maxWidth := none)

      "max-width:100px" `isRenderedFrom` (maxWidth := px 100)

      "max-width:50%" `isRenderedFrom` (maxWidth := pct 50)

      "max-width:calc(100px + 50%)"
        `isRenderedFrom`
          (maxWidth := px 100 @+@ pct 50)

      "max-width:max-content" `isRenderedFrom` (maxWidth := maxContent)

      "max-width:max-content" `isRenderedFrom` (maxWidth := maxContent)

      "max-width:fit-content(100px)"
        `isRenderedFrom`
          (maxWidth := fitContent (px 100))

      "max-width:fit-content(50%)"
        `isRenderedFrom`
          (maxWidth := fitContent (pct 50))

      "max-width:fit-content(calc(100px + 50%))"
        `isRenderedFrom`
          (maxWidth := fitContent (px 100 @+@ pct 50))

    describe "max-height property" do

      "max-height:inherit" `isRenderedFrom` (maxHeight := inherit)

      "max-height:initial" `isRenderedFrom` (maxHeight := initial)

      "max-height:unset" `isRenderedFrom` (maxHeight := unset)

      "max-height:none" `isRenderedFrom` (maxHeight := none)

      "max-height:100px" `isRenderedFrom` (maxHeight := px 100)

      "max-height:50%" `isRenderedFrom` (maxHeight := pct 50)

      "max-height:calc(100px + 50%)"
        `isRenderedFrom`
          (maxHeight := px 100 @+@ pct 50)

      "max-height:max-content" `isRenderedFrom` (maxHeight := maxContent)

      "max-height:max-content" `isRenderedFrom` (maxHeight := maxContent)

      "max-height:fit-content(100px)"
        `isRenderedFrom`
          (maxHeight := fitContent (px 100))

      "max-height:fit-content(50%)"
        `isRenderedFrom`
          (maxHeight := fitContent (pct 50))

      "max-height:fit-content(calc(100px + 50%))"
        `isRenderedFrom`
          (maxHeight := fitContent (px 100 @+@ pct 50))

    describe "box-sizing property" do

      "box-sizing:inherit" `isRenderedFrom` (boxSizing := inherit)

      "box-sizing:initial" `isRenderedFrom` (boxSizing := initial)

      "box-sizing:unset" `isRenderedFrom` (boxSizing := unset)

      "box-sizing:content-box" `isRenderedFrom` (boxSizing := contentBox)

      "box-sizing:border-box" `isRenderedFrom` (boxSizing := borderBox)