module Test.BoxSizingSpec where

import Prelude

import PSCSS (auto, inherit, initial, pct, px, unset, (@+@))
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
