module Test.ColorSpec where

import Prelude

import Color (hsl)
import PSCSS (currentColor, inherit, initial, transparent, unset)
import Test.Spec (Spec, describe)
import Test.Util (renders)

spec :: Spec Unit
spec =
  describe "Color Module" do
    describe "color property" do
      { color: inherit } `renders` "color:inherit"
      { color: initial } `renders` "color:initial"
      { color: unset } `renders` "color:unset"
      { color: transparent } `renders` "color:transparent"
      { color: currentColor } `renders` "color:currentColor"
      { color: hsl 240.0 1.0 0.5 } `renders` "color:#0000ff"
    describe "opacity property" do
      { opacity: inherit } `renders` "opacity:inherit"
      { opacity: initial } `renders` "opacity:initial"
      { opacity: unset } `renders` "opacity:unset"
      { opacity: 0.5 } `renders` "opacity:0.5"
