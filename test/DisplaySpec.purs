-- https://www.w3.org/TR/css-display-3/

module Test.DisplaySpec where

import Prelude

import Tecton
  ( block
  , contents
  , display
  , flex
  , flowRoot
  , grid
  , inherit
  , initial
  , inline
  , inlineBlock
  , inlineFlex
  , inlineGrid
  , inlineTable
  , listItem
  , none
  , table
  , tableCaption
  , tableCell
  , tableColumn
  , tableColumnGroup
  , tableFooterGroup
  , tableHeaderGroup
  , tableRow
  , tableRowGroup
  , unset
  , (:=)
  )
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFromInline)

spec :: Spec Unit
spec = do

  let isRenderedFrom = isRenderedFromInline

  describe "Display Module" do

    describe "display property" do

      "display:inherit" `isRenderedFrom` (display := inherit)

      "display:initial" `isRenderedFrom` (display := initial)

      "display:unset" `isRenderedFrom` (display := unset)

      "display:block" `isRenderedFrom` (display := block)

      "display:inline" `isRenderedFrom` (display := inline)

      "display:flow-root" `isRenderedFrom` (display := flowRoot)

      "display:table" `isRenderedFrom` (display := table)

      "display:flex" `isRenderedFrom` (display := flex)

      "display:grid" `isRenderedFrom` (display := grid)

      "display:list-item" `isRenderedFrom` (display := listItem)

      "display:table-row-group" `isRenderedFrom` (display := tableRowGroup)

      "display:table-header-group"
        `isRenderedFrom`
          (display := tableHeaderGroup)

      "display:table-footer-group"
        `isRenderedFrom`
          (display := tableFooterGroup)

      "display:table-row" `isRenderedFrom` (display := tableRow)

      "display:table-cell" `isRenderedFrom` (display := tableCell)

      "display:table-column-group"
        `isRenderedFrom`
          (display := tableColumnGroup)

      "display:table-column" `isRenderedFrom` (display := tableColumn)

      "display:table-caption" `isRenderedFrom` (display := tableCaption)

      "display:contents" `isRenderedFrom` (display := contents)

      "display:none" `isRenderedFrom` (display := none)

      "display:inline-block" `isRenderedFrom` (display := inlineBlock)

      "display:inline-table" `isRenderedFrom` (display := inlineTable)

      "display:inline-flex" `isRenderedFrom` (display := inlineFlex)

      "display:inline-grid" `isRenderedFrom` (display := inlineGrid)
