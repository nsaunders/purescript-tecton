-- https://www.w3.org/TR/mediaqueries-3/

module Test.MediaQueriesSpec where

import Prelude

import PSCSS (all, landscape, media, portrait, px, universal, (?))
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFrom)

spec :: Spec Unit
spec =
  describe "Media Queries Module" do

    describe "Media features" do

      describe "width" do

        "@media all and (width:600px){*{}}"
          `isRenderedFrom` do
          media all { width: px 600 } ?
            universal ? {}

        "@media all and (min-width:400px) and (max-width:999px){*{}}"
          `isRenderedFrom` do
          media all { minWidth: px 400, maxWidth: px 999 } ?
            universal ? {}

      describe "height" do

        "@media all and (height:600px){*{}}"
          `isRenderedFrom` do
          media all { height: px 600 } ?
            universal ? {}

        "@media all and (min-height:800px) and (max-height:1600px){*{}}"
          `isRenderedFrom` do
          media all { minHeight: px 800, maxHeight: px 1600 } ?
            universal ? {}

      describe "device-width" do

        "@media all and (device-width:600px){*{}}"
          `isRenderedFrom` do
          media all { deviceWidth: px 600 } ?
            universal ? {}

        "@media all and (min-device-width:400px) and (max-device-width:999px){*{}}"
          `isRenderedFrom` do
          media all { minDeviceWidth: px 400, maxDeviceWidth: px 999 } ?
            universal ? {}

      describe "device-height" do

        "@media all and (device-height:600px){*{}}"
          `isRenderedFrom` do
          media all { deviceHeight: px 600 } ?
            universal ? {}

        "@media all and (min-device-height:800px) and (max-device-height:1600px){*{}}"
          `isRenderedFrom` do
          media all { minDeviceHeight: px 800, maxDeviceHeight: px 1600 } ?
            universal ? {}

      describe "orientation" do

        "@media all and (orientation:landscape){*{}}"
          `isRenderedFrom` do
          media all { orientation: landscape } ?
            universal ? {}

        "@media all and (orientation:portrait){*{}}"
          `isRenderedFrom` do
          media all { orientation: portrait } ?
            universal ? {}
