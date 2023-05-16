-- https://www.w3.org/TR/css-ui-4/

module Test.UISpec where

import Prelude

import Color (rgb)
import Data.Tuple.Nested ((/\))
import Tecton
  ( alias
  , allScroll
  , appearance
  , auto
  , cell
  , colResize
  , contextMenu
  , copy
  , crosshair
  , cursor
  , dashed
  , default
  , dotted
  , double
  , eResize
  , ewResize
  , grab
  , grabbing
  , groove
  , help
  , inherit
  , initial
  , inset
  , invert
  , medium
  , menulistButton
  , move
  , nResize
  , neResize
  , neswResize
  , nil
  , noDrop
  , none
  , notAllowed
  , nsResize
  , nwResize
  , nwseResize
  , outlineColor
  , outlineOffset
  , outlineStyle
  , outlineWidth
  , outset
  , pointer
  , progress
  , px
  , ridge
  , rowResize
  , sResize
  , seResize
  , solid
  , swResize
  , text
  , textfield
  , thick
  , thin
  , transparent
  , unset
  , url
  , verticalText
  , wResize
  , wait
  , zoomIn
  , zoomOut
  , (:=)
  , (~)
  )
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFromInline)

spec :: Spec Unit
spec = do

  let isRenderedFrom = isRenderedFromInline

  describe "Basic User Interface Module" do

    describe "outline-width property" do

      "outline-width:inherit" `isRenderedFrom` (outlineWidth := inherit)

      "outline-width:initial" `isRenderedFrom` (outlineWidth := initial)

      "outline-width:unset" `isRenderedFrom` (outlineWidth := unset)

      "outline-width:1px" `isRenderedFrom` (outlineWidth := px 1)

      "outline-width:thin" `isRenderedFrom` (outlineWidth := thin)

      "outline-width:medium" `isRenderedFrom` (outlineWidth := medium)

      "outline-width:thick" `isRenderedFrom` (outlineWidth := thick)

    describe "outline-style property" do

      "outline-style:inherit" `isRenderedFrom` (outlineStyle := inherit)

      "outline-style:initial" `isRenderedFrom` (outlineStyle := initial)

      "outline-style:unset" `isRenderedFrom` (outlineStyle := unset)

      "outline-style:auto" `isRenderedFrom` (outlineStyle := auto)

      "outline-style:none" `isRenderedFrom` (outlineStyle := none)

      "outline-style:dotted" `isRenderedFrom` (outlineStyle := dotted)

      "outline-style:dashed" `isRenderedFrom` (outlineStyle := dashed)

      "outline-style:solid" `isRenderedFrom` (outlineStyle := solid)

      "outline-style:double" `isRenderedFrom` (outlineStyle := double)

      "outline-style:groove" `isRenderedFrom` (outlineStyle := groove)

      "outline-style:ridge" `isRenderedFrom` (outlineStyle := ridge)

      "outline-style:inset" `isRenderedFrom` (outlineStyle := inset)

      "outline-style:outset" `isRenderedFrom` (outlineStyle := outset)

    describe "outline-color property" do

      "outline-color:inherit" `isRenderedFrom` (outlineColor := inherit)

      "outline-color:initial" `isRenderedFrom` (outlineColor := initial)

      "outline-color:unset" `isRenderedFrom` (outlineColor := unset)

      "outline-color:#0000ff" `isRenderedFrom` (outlineColor := rgb 0 0 255)

      "outline-color:transparent" `isRenderedFrom` (outlineColor := transparent)

      "outline-color:invert" `isRenderedFrom` (outlineColor := invert)

    describe "outline-offset property" do

      "outline-offset:inherit" `isRenderedFrom` (outlineOffset := inherit)

      "outline-offset:initial" `isRenderedFrom` (outlineOffset := initial)

      "outline-offset:unset" `isRenderedFrom` (outlineOffset := unset)

      "outline-offset:4px" `isRenderedFrom` (outlineOffset := px 4)

      "outline-offset:0" `isRenderedFrom` (outlineOffset := nil)

    describe "cursor" do

      "cursor:inherit" `isRenderedFrom` (cursor := inherit)

      "cursor:initial" `isRenderedFrom` (cursor := initial)

      "cursor:unset" `isRenderedFrom` (cursor := unset)

      "cursor:auto" `isRenderedFrom` (cursor := auto)

      "cursor:default" `isRenderedFrom` (cursor := default)

      "cursor:none" `isRenderedFrom` (cursor := none)

      "cursor:context-menu" `isRenderedFrom` (cursor := contextMenu)

      "cursor:help" `isRenderedFrom` (cursor := help)

      "cursor:pointer" `isRenderedFrom` (cursor := pointer)

      "cursor:progress" `isRenderedFrom` (cursor := progress)

      "cursor:wait" `isRenderedFrom` (cursor := wait)

      "cursor:cell" `isRenderedFrom` (cursor := cell)

      "cursor:crosshair" `isRenderedFrom` (cursor := crosshair)

      "cursor:text" `isRenderedFrom` (cursor := text)

      "cursor:vertical-text" `isRenderedFrom` (cursor := verticalText)

      "cursor:alias" `isRenderedFrom` (cursor := alias)

      "cursor:copy" `isRenderedFrom` (cursor := copy)

      "cursor:move" `isRenderedFrom` (cursor := move)

      "cursor:no-drop" `isRenderedFrom` (cursor := noDrop)

      "cursor:not-allowed" `isRenderedFrom` (cursor := notAllowed)

      "cursor:grab" `isRenderedFrom` (cursor := grab)

      "cursor:grabbing" `isRenderedFrom` (cursor := grabbing)

      "cursor:e-resize" `isRenderedFrom` (cursor := eResize)

      "cursor:n-resize" `isRenderedFrom` (cursor := nResize)

      "cursor:ne-resize" `isRenderedFrom` (cursor := neResize)

      "cursor:nw-resize" `isRenderedFrom` (cursor := nwResize)

      "cursor:s-resize" `isRenderedFrom` (cursor := sResize)

      "cursor:se-resize" `isRenderedFrom` (cursor := seResize)

      "cursor:sw-resize" `isRenderedFrom` (cursor := swResize)

      "cursor:w-resize" `isRenderedFrom` (cursor := wResize)

      "cursor:ew-resize" `isRenderedFrom` (cursor := ewResize)

      "cursor:ns-resize" `isRenderedFrom` (cursor := nsResize)

      "cursor:nesw-resize" `isRenderedFrom` (cursor := neswResize)

      "cursor:nwse-resize" `isRenderedFrom` (cursor := nwseResize)

      "cursor:col-resize" `isRenderedFrom` (cursor := colResize)

      "cursor:row-resize" `isRenderedFrom` (cursor := rowResize)

      "cursor:all-scroll" `isRenderedFrom` (cursor := allScroll)

      "cursor:zoom-in" `isRenderedFrom` (cursor := zoomIn)

      "cursor:zoom-out" `isRenderedFrom` (cursor := zoomOut)

      "cursor:url(\"example.svg#linkcursor\"),url(\"hyper.cur\"),url(\"hyper.png\") 2 3,pointer"
        `isRenderedFrom`
          ( cursor := url "example.svg#linkcursor" /\ url "hyper.cur"
              /\ url "hyper.png"
                ~ 2
                ~ 3
              /\ pointer
          )

    describe "appearance property" do

      "appearance:inherit" `isRenderedFrom` (appearance := inherit)

      "appearance:initial" `isRenderedFrom` (appearance := initial)

      "appearance:unset" `isRenderedFrom` (appearance := unset)

      "appearance:none" `isRenderedFrom` (appearance := none)

      "appearance:auto" `isRenderedFrom` (appearance := auto)

      "appearance:textfield" `isRenderedFrom` (appearance := textfield)

      "appearance:menulist-button"
        `isRenderedFrom`
          (appearance := menulistButton)