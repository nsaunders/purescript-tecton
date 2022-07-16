-- https://www.w3.org/TR/mediaqueries-3/

module Test.MediaQueriesSpec where

import Prelude

import PSCSS (all, dpcm, dpi, landscape, media, portrait, px, universal, (?), (:/))
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFrom)

spec :: Spec Unit
spec =
  describe "Media Queries Module" do

    describe "Media features" do

      describe "width" do

        "@media all and (width:600px){*{}}"
          `isRenderedFrom` do
          media all { width: px 600 } ? universal ? {}

        "@media all and (min-width:400px) and (max-width:999px){*{}}"
          `isRenderedFrom` do
          media all { minWidth: px 400, maxWidth: px 999 } ? universal ? {}

      describe "height" do

        "@media all and (height:600px){*{}}"
          `isRenderedFrom` do
          media all { height: px 600 } ? universal ? {}

        "@media all and (min-height:800px) and (max-height:1600px){*{}}"
          `isRenderedFrom` do
          media all { minHeight: px 800, maxHeight: px 1600 } ? universal ? {}

      describe "device-width" do

        "@media all and (device-width:600px){*{}}"
          `isRenderedFrom` do
          media all { deviceWidth: px 600 } ? universal ? {}

        "@media all and (min-device-width:400px) and (max-device-width:999px){*{}}"
          `isRenderedFrom` do
          media all { minDeviceWidth: px 400, maxDeviceWidth: px 999 } ?
            universal ? {}

      describe "device-height" do

        "@media all and (device-height:600px){*{}}"
          `isRenderedFrom` do
          media all { deviceHeight: px 600 } ? universal ? {}

        "@media all and (min-device-height:800px) and (max-device-height:1600px){*{}}"
          `isRenderedFrom` do
          media all { minDeviceHeight: px 800, maxDeviceHeight: px 1600 } ?
            universal ? {}

      describe "orientation" do

        "@media all and (orientation:landscape){*{}}"
          `isRenderedFrom` do
          media all { orientation: landscape } ? universal ? {}

        "@media all and (orientation:portrait){*{}}"
          `isRenderedFrom` do
          media all { orientation: portrait } ? universal ? {}

      describe "aspect-ratio" do

        "@media all and (aspect-ratio:16/9){*{}}"
          `isRenderedFrom` do
          media all { aspectRatio: 16 :/ 9 } ? universal ? {}

        "@media all and (min-aspect-ratio:4/3) and (max-aspect-ratio:16/9){*{}}"
          `isRenderedFrom` do
          media all { minAspectRatio: 4 :/ 3, maxAspectRatio: 16 :/ 9 } ?
            universal ? {}

      describe "device-aspect-ratio" do

        "@media all and (device-aspect-ratio:16/9){*{}}"
          `isRenderedFrom` do
          media all { deviceAspectRatio: 16 :/ 9 } ? universal ? {}

        "@media all and (min-device-aspect-ratio:4/3) and (max-device-aspect-ratio:16/9){*{}}"
          `isRenderedFrom` do
          media
            all
            { minDeviceAspectRatio: 4 :/ 3, maxDeviceAspectRatio: 16 :/ 9 } ?
            universal ? {}

      describe "color" do

        "@media all and (color:0){*{}}"
          `isRenderedFrom` do
          media all { color: 0 } ? universal ? {}

        "@media all and (min-color:0) and (max-color:2){*{}}"
          `isRenderedFrom` do
          media all { minColor: 0, maxColor: 2 } ? universal ? {}

      describe "color-index" do

        "@media all and (color-index:1){*{}}"
          `isRenderedFrom` do
          media all { colorIndex: 1 } ? universal ? {}

        "@media all and (min-color-index:1) and (max-color-index:256){*{}}"
          `isRenderedFrom` do
          media all { minColorIndex: 1, maxColorIndex: 256 } ? universal ? {}

      describe "monochrome" do

        "@media all and (monochrome:0){*{}}"
          `isRenderedFrom` do
          media all { monochrome: 0 } ? universal ? {}

        "@media all and (min-monochrome:1) and (max-monochrome:2){*{}}"
          `isRenderedFrom` do
          media all { minMonochrome: 1, maxMonochrome: 2 } ? universal ? {}

      describe "resolution" do

        "@media all and (resolution:300dpi){*{}}"
          `isRenderedFrom` do
          media all { resolution: dpi 300 } ? universal ? {}

        "@media all and (min-resolution:118dpcm) and (max-resolution:236dpcm){*{}}"
          `isRenderedFrom` do
          media all { minResolution: dpcm 118, maxResolution: dpcm 236 } ?
            universal ? {}
