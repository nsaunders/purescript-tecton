-- https://www.w3.org/TR/css-ui-4/

module Test.UISpec where

import Prelude

import Color (rgb)
import Tecton (auto, dashed, dotted, double, groove, inherit, initial, inset, invert, medium, nil, none, outlineColor, outlineOffset, outlineStyle, outlineWidth, outset, px, ridge, solid, thick, thin, transparent, unset, (:=))
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFrom)

spec :: Spec Unit
spec =
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
