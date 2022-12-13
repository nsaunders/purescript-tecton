-- https://www.w3.org/TR/css-position-3/

module Test.PositionSpec where

import Prelude hiding (bottom, top)

import Tecton
  ( absolute
  , auto
  , bottom
  , fixed
  , inherit
  , initial
  , inset
  , insetBlock
  , insetBlockEnd
  , insetBlockStart
  , insetInline
  , insetInlineEnd
  , insetInlineStart
  , left
  , nil
  , pct
  , position
  , px
  , relative
  , right
  , static
  , sticky
  , top
  , unset
  , (:=)
  , (~)
  )
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFromInline)

spec :: Spec Unit
spec = do

  let isRenderedFrom = isRenderedFromInline

  describe "Positioned Layout Module" do

    describe "position property" do

      "position:inherit" `isRenderedFrom` (position := inherit)

      "position:initial" `isRenderedFrom` (position := initial)

      "position:unset" `isRenderedFrom` (position := unset)

      "position:static" `isRenderedFrom` (position := static)

      "position:relative" `isRenderedFrom` (position := relative)

      "position:absolute" `isRenderedFrom` (position := absolute)

      "position:sticky" `isRenderedFrom` (position := sticky)

      "position:fixed" `isRenderedFrom` (position := fixed)

    describe "top property" do

      "top:inherit" `isRenderedFrom` (top := inherit)

      "top:initial" `isRenderedFrom` (top := initial)

      "top:unset" `isRenderedFrom` (top := unset)

      "top:auto" `isRenderedFrom` (top := auto)

      "top:0" `isRenderedFrom` (top := nil)

      "top:8px" `isRenderedFrom` (top := px 8)

      "top:10%" `isRenderedFrom` (top := pct 10)

    describe "right property" do

      "right:inherit" `isRenderedFrom` (right := inherit)

      "right:initial" `isRenderedFrom` (right := initial)

      "right:unset" `isRenderedFrom` (right := unset)

      "right:auto" `isRenderedFrom` (right := auto)

      "right:0" `isRenderedFrom` (right := nil)

      "right:8px" `isRenderedFrom` (right := px 8)

      "right:10%" `isRenderedFrom` (right := pct 10)

    describe "bottom property" do

      "bottom:inherit" `isRenderedFrom` (bottom := inherit)

      "bottom:initial" `isRenderedFrom` (bottom := initial)

      "bottom:unset" `isRenderedFrom` (bottom := unset)

      "bottom:auto" `isRenderedFrom` (bottom := auto)

      "bottom:0" `isRenderedFrom` (bottom := nil)

      "bottom:8px" `isRenderedFrom` (bottom := px 8)

      "bottom:10%" `isRenderedFrom` (bottom := pct 10)

    describe "left property" do

      "left:inherit" `isRenderedFrom` (left := inherit)

      "left:initial" `isRenderedFrom` (left := initial)

      "left:unset" `isRenderedFrom` (left := unset)

      "left:auto" `isRenderedFrom` (left := auto)

      "left:0" `isRenderedFrom` (left := nil)

      "left:8px" `isRenderedFrom` (left := px 8)

      "left:10%" `isRenderedFrom` (left := pct 10)

    describe "inset-block-start property" do

      "inset-block-start:inherit" `isRenderedFrom` (insetBlockStart := inherit)

      "inset-block-start:initial" `isRenderedFrom` (insetBlockStart := initial)

      "inset-block-start:unset" `isRenderedFrom` (insetBlockStart := unset)

      "inset-block-start:auto" `isRenderedFrom` (insetBlockStart := auto)

      "inset-block-start:0" `isRenderedFrom` (insetBlockStart := nil)

      "inset-block-start:8px" `isRenderedFrom` (insetBlockStart := px 8)

      "inset-block-start:10%" `isRenderedFrom` (insetBlockStart := pct 10)

    describe "inset-inline-start property" do

      "inset-inline-start:inherit"
        `isRenderedFrom`
          (insetInlineStart := inherit)

      "inset-inline-start:initial"
        `isRenderedFrom`
          (insetInlineStart := initial)

      "inset-inline-start:unset" `isRenderedFrom` (insetInlineStart := unset)

      "inset-inline-start:auto" `isRenderedFrom` (insetInlineStart := auto)

      "inset-inline-start:0" `isRenderedFrom` (insetInlineStart := nil)

      "inset-inline-start:8px" `isRenderedFrom` (insetInlineStart := px 8)

      "inset-inline-start:10%" `isRenderedFrom` (insetInlineStart := pct 10)

    describe "inset-block-end property" do

      "inset-block-end:inherit" `isRenderedFrom` (insetBlockEnd := inherit)

      "inset-block-end:initial" `isRenderedFrom` (insetBlockEnd := initial)

      "inset-block-end:unset" `isRenderedFrom` (insetBlockEnd := unset)

      "inset-block-end:auto" `isRenderedFrom` (insetBlockEnd := auto)

      "inset-block-end:0" `isRenderedFrom` (insetBlockEnd := nil)

      "inset-block-end:8px" `isRenderedFrom` (insetBlockEnd := px 8)

      "inset-block-end:10%" `isRenderedFrom` (insetBlockEnd := pct 10)

    describe "inset-inline-end property" do

      "inset-inline-end:inherit" `isRenderedFrom` (insetInlineEnd := inherit)

      "inset-inline-end:initial" `isRenderedFrom` (insetInlineEnd := initial)

      "inset-inline-end:unset" `isRenderedFrom` (insetInlineEnd := unset)

      "inset-inline-end:auto" `isRenderedFrom` (insetInlineEnd := auto)

      "inset-inline-end:0" `isRenderedFrom` (insetInlineEnd := nil)

      "inset-inline-end:8px" `isRenderedFrom` (insetInlineEnd := px 8)

      "inset-inline-end:10%" `isRenderedFrom` (insetInlineEnd := pct 10)

    describe "inset-block" do

      "inset-block:inherit" `isRenderedFrom` (insetBlock := inherit)

      "inset-block:initial" `isRenderedFrom` (insetBlock := initial)

      "inset-block:unset" `isRenderedFrom` (insetBlock := unset)

      "inset-block:auto" `isRenderedFrom` (insetBlock := auto)

      "inset-block:10px" `isRenderedFrom` (insetBlock := px 10)

      "inset-block:1%" `isRenderedFrom` (insetBlock := pct 1)

      "inset-block:auto 1px" `isRenderedFrom` (insetBlock := auto ~ px 1)

      "inset-block:10px 1%" `isRenderedFrom` (insetBlock := px 10 ~ pct 1)

      "inset-block:10% auto" `isRenderedFrom` (insetBlock := pct 10 ~ auto)

      "inset-block:1% 10px" `isRenderedFrom` (insetBlock := pct 1 ~ px 10)

    describe "inset-inline" do

      "inset-inline:inherit" `isRenderedFrom` (insetInline := inherit)

      "inset-inline:initial" `isRenderedFrom` (insetInline := initial)

      "inset-inline:unset" `isRenderedFrom` (insetInline := unset)

      "inset-inline:auto" `isRenderedFrom` (insetInline := auto)

      "inset-inline:10px" `isRenderedFrom` (insetInline := px 10)

      "inset-inline:1%" `isRenderedFrom` (insetInline := pct 1)

      "inset-inline:auto 1px" `isRenderedFrom` (insetInline := auto ~ px 1)

      "inset-inline:10px 1%" `isRenderedFrom` (insetInline := px 10 ~ pct 1)

      "inset-inline:10% auto" `isRenderedFrom` (insetInline := pct 10 ~ auto)

      "inset-inline:1% 10px" `isRenderedFrom` (insetInline := pct 1 ~ px 10)

    describe "inset" do

      "inset:inherit" `isRenderedFrom` (inset := inherit)

      "inset:initial" `isRenderedFrom` (inset := initial)

      "inset:unset" `isRenderedFrom` (inset := unset)

      "inset:auto" `isRenderedFrom` (inset := auto)

      "inset:10px" `isRenderedFrom` (inset := px 10)

      "inset:1%" `isRenderedFrom` (inset := pct 1)

      "inset:1% 1px" `isRenderedFrom` (inset := pct 1 ~ px 1)

      "inset:1px auto" `isRenderedFrom` (inset := px 1 ~ auto)

      "inset:auto 1%" `isRenderedFrom` (inset := auto ~ pct 1)

      "inset:1% 1px auto" `isRenderedFrom` (inset := pct 1 ~ px 1 ~ auto)

      "inset:1px auto 1%" `isRenderedFrom` (inset := px 1 ~ auto ~ pct 1)

      "inset:auto 1% 1px" `isRenderedFrom` (inset := auto ~ pct 1 ~ px 1)

      "inset:1% 1px auto 2%"
        `isRenderedFrom`
          (inset := pct 1 ~ px 1 ~ auto ~ pct 2)

      "inset:2% 1% 1px auto"
        `isRenderedFrom`
          (inset := pct 2 ~ pct 1 ~ px 1 ~ auto)

      "inset:auto 2% 1% 1px"
        `isRenderedFrom`
          (inset := auto ~ pct 2 ~ pct 1 ~ px 1)

      "inset:1px auto 2% 1%"
        `isRenderedFrom`
          (inset := px 1 ~ auto ~ pct 2 ~ pct 1)
