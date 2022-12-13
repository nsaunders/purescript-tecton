-- https://www.w3.org/TR/mediaqueries-3/

module Test.MediaQueriesSpec where

import Prelude

import Tecton
  ( all
  , dpcm
  , dpi
  , landscape
  , media
  , nil
  , portrait
  , print
  , px
  , screen
  , universal
  , width
  , (:/)
  , (:=)
  , (?)
  )
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFromSheet)

spec :: Spec Unit
spec = do

  let isRenderedFrom = isRenderedFromSheet

  describe "Media Queries Module" do

    describe "Media types" do

      "@media all{*{width:0}}"
        `isRenderedFrom` do
          media all {} ? universal ? width := nil

      "@media screen{*{width:0}}"
        `isRenderedFrom` do
          media screen {} ? universal ? width := nil

      "@media print{*{width:0}}"
        `isRenderedFrom` do
          media print {} ? universal ? width := nil

      describe "width" do

        "@media all and (width:600px){*{width:0}}"
          `isRenderedFrom` do
            media all { width: px 600 } ? universal ? width := nil

        "@media all and (max-width:999px) and (min-width:400px){*{width:0}}"
          `isRenderedFrom` do
            media all { minWidth: px 400, maxWidth: px 999 } ? do
              universal ? width := nil

      describe "height" do

        "@media all and (height:600px){*{width:0}}"
          `isRenderedFrom` do
            media all { height: px 600 } ? universal ? width := nil

        "@media all and (max-height:1600px) and (min-height:800px){*{width:0}}"
          `isRenderedFrom` do
            media all { minHeight: px 800, maxHeight: px 1600 } ? do
              universal ? width := nil

      describe "device-width" do

        "@media all and (device-width:600px){*{width:0}}"
          `isRenderedFrom` do
            media all { deviceWidth: px 600 } ? universal ? width := nil

        "@media all and (max-device-width:999px) and (min-device-width:400px){*{width:0}}"
          `isRenderedFrom` do
            media all { minDeviceWidth: px 400, maxDeviceWidth: px 999 } ? do
              universal ? width := nil

      describe "device-height" do

        "@media all and (device-height:600px){*{width:0}}"
          `isRenderedFrom` do
            media all { deviceHeight: px 600 } ? universal ? width := nil

        "@media all and (max-device-height:1600px) and (min-device-height:800px){*{width:0}}"
          `isRenderedFrom` do
            media all { minDeviceHeight: px 800, maxDeviceHeight: px 1600 } ? do
              universal ? width := nil

      describe "orientation" do

        "@media all and (orientation:landscape){*{width:0}}"
          `isRenderedFrom` do
            media all { orientation: landscape } ? universal ? width := nil

        "@media all and (orientation:portrait){*{width:0}}"
          `isRenderedFrom` do
            media all { orientation: portrait } ? universal ? width := nil

      describe "aspect-ratio" do

        "@media all and (aspect-ratio:16/9){*{width:0}}"
          `isRenderedFrom` do
            media all { aspectRatio: 16 :/ 9 } ? universal ? width := nil

        "@media all and (max-aspect-ratio:16/9) and (min-aspect-ratio:4/3){*{width:0}}"
          `isRenderedFrom` do
            media all { minAspectRatio: 4 :/ 3, maxAspectRatio: 16 :/ 9 } ? do
              universal ? width := nil

      describe "device-aspect-ratio" do

        "@media all and (device-aspect-ratio:16/9){*{width:0}}"
          `isRenderedFrom` do
            media all { deviceAspectRatio: 16 :/ 9 } ? universal ? width := nil

        "@media all and (max-device-aspect-ratio:16/9) and (min-device-aspect-ratio:4/3){*{width:0}}"
          `isRenderedFrom` do
            media all
              { minDeviceAspectRatio: 4 :/ 3, maxDeviceAspectRatio: 16 :/ 9 } ?
              do
                universal ? width := nil

      describe "color" do

        "@media all and (color:0){*{width:0}}"
          `isRenderedFrom` do
            media all { color: 0 } ? universal ? width := nil

        "@media all and (max-color:2) and (min-color:0){*{width:0}}"
          `isRenderedFrom` do
            media all { minColor: 0, maxColor: 2 } ? universal ? width := nil

      describe "color-index" do

        "@media all and (color-index:1){*{width:0}}"
          `isRenderedFrom` do
            media all { colorIndex: 1 } ? universal ? width := nil

        "@media all and (max-color-index:256) and (min-color-index:1){*{width:0}}"
          `isRenderedFrom` do
            media all { minColorIndex: 1, maxColorIndex: 256 } ? do
              universal ? width := nil

      describe "monochrome" do

        "@media all and (monochrome:0){*{width:0}}"
          `isRenderedFrom` do
            media all { monochrome: 0 } ? universal ? width := nil

        "@media all and (max-monochrome:2) and (min-monochrome:1){*{width:0}}"
          `isRenderedFrom` do
            media all { minMonochrome: 1, maxMonochrome: 2 } ? do
              universal ? width := nil

      describe "resolution" do

        "@media all and (resolution:300dpi){*{width:0}}"
          `isRenderedFrom` do
            media all { resolution: dpi 300 } ? universal ? width := nil

        "@media all and (max-resolution:236dpcm) and (min-resolution:118dpcm){*{width:0}}"
          `isRenderedFrom` do
            media all { minResolution: dpcm 118, maxResolution: dpcm 236 } ? do
              universal ? width := nil
