-- https://www.w3.org/TR/css-transforms-1/
-- https://www.w3.org/TR/css-transforms-2/

module Test.TransformsSpec where

import Prelude

import Data.Tuple.Nested ((/\))
import PSCSS (deg, inherit, initial, matrix, matrix3d, none, pct, perspective, px, rad, rotate, rotate3d, rotateX, rotateY, rotateZ, scale, scale3d, scaleX, scaleY, scaleZ, skewX, skewY, translate, translate3d, translateX, translateY, translateZ, turn, unset)
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFrom)

spec :: Spec Unit
spec =
  describe "Transforms Module" do

    describe "2D Transform Functions" do

      "matrix(1.2,0.2,-1,0.9,0,20)"
        `isRenderedFrom`
        matrix 1.2 0.2 (-1) 0.9 0 20

      "matrix(0.4,0,0.5,1.2,60,10)"
        `isRenderedFrom`
        matrix 0.4 0 0.5 1.2 60 10

      "matrix(0,1,1,0,0,0)" `isRenderedFrom` matrix 0 1 1 0 0 0

      "matrix(0.1,1,-0.3,1,0,0)" `isRenderedFrom` matrix 0.1 1 (-0.3) 1 0 0

      "translate(10px,10%)" `isRenderedFrom` translate (px 10) (pct 10)

      "translate(10%,10px)" `isRenderedFrom` translate (pct 10) (px 10)

      "translateX(10%)" `isRenderedFrom` translateX (pct 10)

      "translateX(10px)" `isRenderedFrom` translateX (px 10)

      "translateY(10%)" `isRenderedFrom` translateY (pct 10)

      "translateY(10px)" `isRenderedFrom` translateY (px 10)

      "translateY(10px)" `isRenderedFrom` translateY (px 10)

      "scale(0.5,0.75)" `isRenderedFrom` scale 0.5 0.75

      "scaleX(0.5)" `isRenderedFrom` scaleX 0.5

      "scaleY(0.5)" `isRenderedFrom` scaleY 0.5

      "rotate(0.25turn)" `isRenderedFrom` rotate (turn 0.25)

      "skewX(10deg)" `isRenderedFrom` skewX (deg 10)

      "skewY(75deg)" `isRenderedFrom` skewY (deg 75)

    describe "3D Transform Functions" do

      "matrix3d(-0.6,1.34788,0,0,-2.34788,-0.6,0,0,0,0,1,0,0,0,10,1)"
        `isRenderedFrom`
        matrix3d (-0.6) 1.34788 0 0 (-2.34788) (-0.6) 0 0 0 0 1 0 0 0 10 1
 
      "matrix3d(0.5,0,-0.866025,0,0.595877,1.2,-1.03209,0,0.866025,0,0.5,0,25.9808,0,15,1)"
        `isRenderedFrom`
        matrix3d 0.5 0 (-0.866025) 0 0.595877 1.2 (-1.03209) 0 0.866025 0 0.5 0 25.9808 0 15 1

      "translate3d(10%,50px,10px)"
        `isRenderedFrom`
        translate3d (pct 10) (px 50) (px 10)

      "translate3d(50px,10%,10px)"
        `isRenderedFrom`
        translate3d (px 50) (pct 10) (px 10)

      "translateZ(10px)" `isRenderedFrom` translateZ (px 10)

      "scale3d(1,1,1)" `isRenderedFrom` scale3d 1 1 1

      "scale3d(-1.4,0.4,0.7)" `isRenderedFrom` scale3d (-1.4) 0.4 0.7

      "scaleZ(1)" `isRenderedFrom` scaleZ 1

      "scaleZ(-1.4)" `isRenderedFrom` scaleZ (-1.4)

      "rotate3d(1,1,1,45deg)" `isRenderedFrom` rotate3d 1 1 1 (deg 45)

      "rotate3d(2,-1,-1,-0.2turn)"
        `isRenderedFrom`
        rotate3d 2 (-1) (-1) (turn (-0.2))

      "rotate3d(0.5,1.5,0.5,3.142rad)"
        `isRenderedFrom`
        rotate3d 0.5 1.5 0.5 (rad 3.142)

      "rotateX(45deg)" `isRenderedFrom` rotateX (deg 45)

      "rotateY(45deg)" `isRenderedFrom` rotateY (deg 45)

      "rotateZ(45deg)" `isRenderedFrom` rotateZ (deg 45)

      "perspective(50px)" `isRenderedFrom` perspective (px 50)

      "perspective(none)" `isRenderedFrom` perspective none

    describe "transform property" do

      "transform:inherit" `isRenderedFrom` { transform: inherit }

      "transform:initial" `isRenderedFrom` { transform: initial }

      "transform:unset" `isRenderedFrom` { transform: unset }

      "transform:none" `isRenderedFrom` { transform: none }

      "transform:scaleX(1.5)" `isRenderedFrom` { transform: scaleX 1.5 }

      "transform:rotateX(45deg) translateX(-10px) scaleX(1.5)"
        `isRenderedFrom`
        { transform: rotateX (deg 45) /\ translateX (px (-10)) /\ scaleX 1.5 }
