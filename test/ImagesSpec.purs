-- https://www.w3.org/TR/css-images-3/

module Test.ImagesSpec where

import Prelude hiding (top)

import Color (black, rgb, white)
import PSCSS (at, at2, circle, closestSide, currentColor, deg, ellipse, farthestCorner, hint, linearGradient, linearGradient1, pct, px, radialGradient, radialGradient1, radialGradient2, radialGradient3, radialGradient4, repeating, right, stop, stop2, to, to2, top, transparent)
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFrom)

spec :: Spec Unit
spec =
  describe "Images Module" do

    describe "linear-gradient()" do

      "linear-gradient(#0000ff,#008000)"
        `isRenderedFrom` do
        linearGradient # stop (rgb 0 0 255) # stop (rgb 0 128 0)

      "linear-gradient(45deg,#0000ff,#008000)"
        `isRenderedFrom` do
        linearGradient1 (deg 45) # stop (rgb 0 0 255) # stop (rgb 0 128 0)

      "linear-gradient(to top,#0000ff,#008000 40%,#ff0000)"
        `isRenderedFrom` do
        linearGradient1 (to top)
          # stop (rgb 0 0 255)
          # stop2 (rgb 0 128 0) (pct 40)
          # stop (rgb 255 0 0)

      "linear-gradient(to top right,currentColor,10%,transparent)"
        `isRenderedFrom` do
        linearGradient1 (to2 top right)
          # stop currentColor
          # hint (pct 10)
          # stop transparent

    describe "radial-gradient()" do

      "radial-gradient(#000080,30px,#800000)"
        `isRenderedFrom` do
        radialGradient # stop (rgb 0 0 128) # hint (px 30) # stop (rgb 128 0 0)

      "radial-gradient(circle,#000000,#ffffff)"
        `isRenderedFrom` do
        radialGradient1 circle # stop black # stop white

      "radial-gradient(ellipse,#ff9900,#0099ff)"
        `isRenderedFrom` do
        radialGradient1 ellipse # stop (rgb 255 153 0) # stop (rgb 0 153 255)

      "radial-gradient(farthest-corner,#ffff00,#000000)"
        `isRenderedFrom` do
        radialGradient1 farthestCorner # stop (rgb 255 255 0) # stop black

      "radial-gradient(at top,#99ff00,#ffff00)"
        `isRenderedFrom` do
        radialGradient1 (at top) # stop (rgb 153 255 0) # stop (rgb 255 255 0)

      "radial-gradient(10px,#009900,#009999)"
        `isRenderedFrom` do
        radialGradient1 (px 10) # stop (rgb 0 153 0) # stop (rgb 0 153 153)

      "radial-gradient(circle farthest-corner,#000000,#ffffff)"
        `isRenderedFrom` do
        radialGradient2 circle farthestCorner # stop black # stop white

      "radial-gradient(ellipse farthest-corner,#808000,#800080)"
        `isRenderedFrom` do
        radialGradient2 ellipse farthestCorner
          # stop (rgb 128 128 0)
          # stop (rgb 128 0 128)

      "radial-gradient(circle at top,#ff0000,#0000ff)"
        `isRenderedFrom` do
        radialGradient2 circle (at top)
          # stop (rgb 255 0 0)
          # stop (rgb 0 0 255)

      "radial-gradient(ellipse at top,transparent,currentColor)"
        `isRenderedFrom` do
        radialGradient2 ellipse (at top)
          # stop transparent
          # stop currentColor

      "radial-gradient(circle 10px,#ffff00,#ff00ff)"
        `isRenderedFrom` do
        radialGradient2 circle (px 10)
          # stop (rgb 255 255 0)
          # stop (rgb 255 0 255)

      "radial-gradient(10% 20px,#000000,transparent)"
        `isRenderedFrom` do
        radialGradient2 (pct 10) (px 20) # stop black # stop transparent

      "radial-gradient(circle farthest-corner at top,#000000,#ffffff)"
        `isRenderedFrom` do
        radialGradient3 circle farthestCorner (at top)
          # stop black
          # stop white

      "radial-gradient(ellipse farthest-corner at top,transparent,currentColor)"
        `isRenderedFrom` do
        radialGradient3 ellipse farthestCorner (at top)
          # stop transparent
          # stop currentColor

      "radial-gradient(circle 10px at top,transparent,#ffff00)"
        `isRenderedFrom` do
        radialGradient3 circle (px 10) (at top)
          # stop transparent
          # stop (rgb 255 255 0)

      "radial-gradient(ellipse 10px 20%,#008000,#ffffff)"
        `isRenderedFrom` do
        radialGradient3 ellipse (px 10) (pct 20)
          # stop (rgb 0 128 0)
          # stop white

      "radial-gradient(10% 20% at top,#ff0000,#0000ff)"
        `isRenderedFrom` do
        radialGradient3 (pct 10) (pct 20) (at top)
          # stop (rgb 255 0 0)
          # stop (rgb 0 0 255)

      "radial-gradient(ellipse 5% 10px at top,#000000,#ffffff)"
        `isRenderedFrom` do
        radialGradient4 ellipse (pct 5) (px 10) (at top)
          # stop black
          # stop white

    describe "repeating-linear-gradient()" do
 
      "repeating-linear-gradient(#ff0000,#0000ff 20px,#ff0000 40px)"
        `isRenderedFrom` do
        repeating linearGradient
          # stop (rgb 255 0 0)
          # stop2 (rgb 0 0 255) (px 20)
          # stop2 (rgb 255 0 0) (px 40)

      "repeating-linear-gradient(#ff0000,#0000ff 20px,#ff0000 40px)"
        `isRenderedFrom` do
        repeating linearGradient
          # stop (rgb 255 0 0)
          # stop2 (rgb 0 0 255) (px 20)
          # stop2 (rgb 255 0 0) (px 40)

      "repeating-linear-gradient(to right,#ff0000,#0000ff 20px,#ff0000 40px)"
        `isRenderedFrom` do
        (repeating $ linearGradient1 $ to right)
          # stop (rgb 255 0 0)
          # stop2 (rgb 0 0 255) (px 20)
          # stop2 (rgb 255 0 0) (px 40)

    describe "repeating-radial-gradient()" do

      "repeating-radial-gradient(circle closest-side at 20px 30px,#ff0000,#ffff00,#00ff00 100%,#ffff00 150%,#ff0000 200%)"
        `isRenderedFrom` do
        (repeating $ radialGradient3 circle closestSide $ at2 (px 20) (px 30))
            # stop (rgb 255 0 0)
            # stop (rgb 255 255 0)
            # stop2 (rgb 0 255 0) (pct 100)
            # stop2 (rgb 255 255 0) (pct 150)
            # stop2 (rgb 255 0 0) (pct 200)
