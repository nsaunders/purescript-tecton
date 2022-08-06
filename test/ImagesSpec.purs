-- https://www.w3.org/TR/css-images-3/

module Test.ImagesSpec where

import Prelude hiding (top)

import Color (black, rgb)
import PSCSS (bottom, center, circle, closestSide, currentColor, deg, ellipse, farthestCorner, hint, left, linearGradient, nil, pct, px, radialGradient, repeating, right, stop, stop2, top, transparent, (~))
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFrom)

spec :: Spec Unit
spec =
  describe "Images Module" do

    describe "linear-gradient()" do

      "linear-gradient(0,#0000ff,#008000)"
        `isRenderedFrom` do
        linearGradient nil # stop (rgb 0 0 255) # stop (rgb 0 128 0)

      "linear-gradient(45deg,#0000ff,#008000)"
        `isRenderedFrom` do
        linearGradient (deg 45) # stop (rgb 0 0 255) # stop (rgb 0 128 0)

      "linear-gradient(90deg,#0000ff,#008000 40%,#ff0000)"
        `isRenderedFrom` do
        linearGradient (deg 90)
          # stop (rgb 0 0 255)
          # stop2 (rgb 0 128 0) (pct 40)
          # stop (rgb 255 0 0)

      "linear-gradient(45deg,currentColor,10%,transparent)"
        `isRenderedFrom` do
        linearGradient (deg 45)
          # stop currentColor
          # hint (pct 10)
          # stop transparent

    describe "radial-gradient()" do

      "radial-gradient(circle at center,#000080,30px,#800000)"
        `isRenderedFrom` do
        radialGradient circle center
          # stop (rgb 0 0 128)
          # hint (px 30)
          # stop (rgb 128 0 0)

      "radial-gradient(ellipse at center,#ff9900,#0099ff)"
        `isRenderedFrom` do
        radialGradient ellipse center
          # stop (rgb 255 153 0)
          # stop (rgb 0 153 255)

      "radial-gradient(circle farthest-corner at center,#ffff00,#000000)"
        `isRenderedFrom` do
        radialGradient (circle ~ farthestCorner) center
          # stop (rgb 255 255 0)
          # stop black

      "radial-gradient(ellipse farthest-corner at left top,#808000,#800080)"
        `isRenderedFrom` do
        radialGradient (ellipse ~ farthestCorner) (left ~ top)
          # stop (rgb 128 128 0)
          # stop (rgb 128 0 128)

      "radial-gradient(10px at left,#009900,#009999)"
        `isRenderedFrom` do
        radialGradient (px 10) left # stop (rgb 0 153 0) # stop (rgb 0 153 153)

      "radial-gradient(10px 20px at right bottom,#000000,transparent)"
        `isRenderedFrom` do
        radialGradient (px 10 ~ px 20) (right ~ bottom)
          # stop black
          # stop transparent

      "radial-gradient(10% 20% at top,#ff0000,#0000ff)"
        `isRenderedFrom` do
        radialGradient (pct 10 ~ pct 20) top
          # stop (rgb 255 0 0)
          # stop (rgb 0 0 255)

    describe "repeating-linear-gradient()" do
 
      "repeating-linear-gradient(0,#ff0000,#0000ff 20px,#ff0000 40px)"
        `isRenderedFrom` do
        (repeating $ linearGradient nil)
          # stop (rgb 255 0 0)
          # stop2 (rgb 0 0 255) (px 20)
          # stop2 (rgb 255 0 0) (px 40)

      "repeating-linear-gradient(90deg,#ff0000,#0000ff 20px,#ff0000 40px)"
        `isRenderedFrom` do
        (repeating $ linearGradient $ deg 90)
          # stop (rgb 255 0 0)
          # stop2 (rgb 0 0 255) (px 20)
          # stop2 (rgb 255 0 0) (px 40)

    describe "repeating-radial-gradient()" do

      "repeating-radial-gradient(circle closest-side at 20px 30px,#ff0000,#ffff00,#00ff00 100%,#ffff00 150%,#ff0000 200%)"
        `isRenderedFrom` do
        (repeating $ radialGradient (circle ~ closestSide) (px 20 ~ px 30))
            # stop (rgb 255 0 0)
            # stop (rgb 255 255 0)
            # stop2 (rgb 0 255 0) (pct 100)
            # stop2 (rgb 255 255 0) (pct 150)
            # stop2 (rgb 255 0 0) (pct 200)
