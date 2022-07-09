module Test.BoxSizingSpec where

import Prelude

import PSCSS (auto, fitContent, inherit, initial, maxContent, minContent, pct, px, unset, (@+@))
import Test.Spec (Spec, describe)
import Test.Util (renders)

spec :: Spec Unit
spec = describe "Box Sizing Module" do
  describe "width property" do
    { width: inherit } `renders` "width:inherit"
    { width: initial } `renders` "width:initial"
    { width: unset } `renders` "width:unset"
    { width: auto } `renders` "width:auto"
    { width: px 100 } `renders` "width:100px"
    { width: pct 50 } `renders` "width:50%"
    { width: px 100 @+@ pct 50 } `renders` "width:calc(100px + 50%)"
    { width: minContent } `renders` "width:min-content"
    { width: maxContent } `renders` "width:max-content"
    { width: fitContent (px 100) } `renders` "width:fit-content(100px)"
    { width: fitContent (pct 50) } `renders` "width:fit-content(50%)"
    { width: fitContent (px 100 @+@ pct 50) }
      `renders`
      "width:fit-content(calc(100px + 50%))"
  describe "height property" do
    { height: inherit } `renders` "height:inherit"
    { height: initial } `renders` "height:initial"
    { height: unset } `renders` "height:unset"
    { height: auto } `renders` "height:auto"
    { height: px 100 } `renders` "height:100px"
    { height: pct 50 } `renders` "height:50%"
    { height: px 100 @+@ pct 50 } `renders` "height:calc(100px + 50%)"
    { height: minContent } `renders` "height:min-content"
    { height: maxContent } `renders` "height:max-content"
    { height: fitContent (px 100) } `renders` "height:fit-content(100px)"
    { height: fitContent (pct 50) } `renders` "height:fit-content(50%)"
    { height: fitContent (px 100 @+@ pct 50) }
      `renders`
      "height:fit-content(calc(100px + 50%))"
