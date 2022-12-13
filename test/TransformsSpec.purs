-- https://www.w3.org/TR/css-transforms-1/
-- https://www.w3.org/TR/css-transforms-2/

module Test.TransformsSpec where

import Prelude hiding (bottom, top)

import Data.Tuple.Nested ((/\))
import Tecton
  ( bottom
  , center
  , deg
  , inherit
  , initial
  , left
  , matrix
  , matrix3d
  , none
  , pct
  , perspective
  , px
  , rad
  , right
  , rotate
  , rotate3d
  , rotateX
  , rotateY
  , rotateZ
  , scale
  , scale3d
  , scaleX
  , scaleY
  , scaleZ
  , skewX
  , skewY
  , top
  , transform
  , transformOrigin
  , translate
  , translate3d
  , translateX
  , translateY
  , translateZ
  , turn
  , unset
  , (:=)
  , (~)
  )
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFromInline, isRenderedFromVal)

spec :: Spec Unit
spec =
  describe "Transforms Module" do

    describe "2D Transform Functions" do

      let isRenderedFrom = isRenderedFromVal

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

      let isRenderedFrom = isRenderedFromVal

      "matrix3d(-0.6,1.34788,0,0,-2.34788,-0.6,0,0,0,0,1,0,0,0,10,1)"
        `isRenderedFrom`
          matrix3d (-0.6) 1.34788 0 0 (-2.34788) (-0.6) 0 0 0 0 1 0 0 0 10 1

      "matrix3d(0.5,0,-0.866025,0,0.595877,1.2,-1.03209,0,0.866025,0,0.5,0,25.9808,0,15,1)"
        `isRenderedFrom`
          matrix3d 0.5 0 (-0.866025) 0 0.595877 1.2 (-1.03209) 0 0.866025 0 0.5
            0
            25.9808
            0
            15
            1

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

      let isRenderedFrom = isRenderedFromInline

      "transform:inherit" `isRenderedFrom` (transform := inherit)

      "transform:initial" `isRenderedFrom` (transform := initial)

      "transform:unset" `isRenderedFrom` (transform := unset)

      "transform:none" `isRenderedFrom` (transform := none)

      "transform:scaleX(1.5)" `isRenderedFrom` (transform := scaleX 1.5)

      "transform:rotateX(45deg) translateX(-10px) scaleX(1.5)"
        `isRenderedFrom`
          (transform := rotateX (deg 45) /\ translateX (px (-10)) /\ scaleX 1.5)

    describe "transform-origin property" do

      let isRenderedFrom = isRenderedFromInline

      "transform-origin:inherit" `isRenderedFrom` (transformOrigin := inherit)

      "transform-origin:initial" `isRenderedFrom` (transformOrigin := initial)

      "transform-origin:unset" `isRenderedFrom` (transformOrigin := unset)

      "transform-origin:left" `isRenderedFrom` (transformOrigin := left)

      "transform-origin:center" `isRenderedFrom` (transformOrigin := center)

      "transform-origin:right" `isRenderedFrom` (transformOrigin := right)

      "transform-origin:top" `isRenderedFrom` (transformOrigin := top)

      "transform-origin:bottom" `isRenderedFrom` (transformOrigin := bottom)

      "transform-origin:10px" `isRenderedFrom` (transformOrigin := px 10)

      "transform-origin:10%" `isRenderedFrom` (transformOrigin := pct 10)

      "transform-origin:left top"
        `isRenderedFrom`
          (transformOrigin := left ~ top)

      "transform-origin:center top"
        `isRenderedFrom`
          (transformOrigin := center ~ top)

      "transform-origin:right top"
        `isRenderedFrom`
          (transformOrigin := right ~ top)

      "transform-origin:left center"
        `isRenderedFrom`
          (transformOrigin := left ~ center)

      "transform-origin:center center"
        `isRenderedFrom`
          (transformOrigin := center ~ center)

      "transform-origin:right center"
        `isRenderedFrom`
          (transformOrigin := right ~ center)

      "transform-origin:left bottom"
        `isRenderedFrom`
          (transformOrigin := left ~ bottom)

      "transform-origin:center bottom"
        `isRenderedFrom`
          (transformOrigin := center ~ bottom)

      "transform-origin:right bottom"
        `isRenderedFrom`
          (transformOrigin := right ~ bottom)

      "transform-origin:10px 20%"
        `isRenderedFrom`
          (transformOrigin := px 10 ~ pct 20)

      "transform-origin:20% 10px"
        `isRenderedFrom`
          (transformOrigin := pct 20 ~ px 10)

      "transform-origin:10px top"
        `isRenderedFrom`
          (transformOrigin := px 10 ~ top)

      "transform-origin:10% bottom"
        `isRenderedFrom`
          (transformOrigin := pct 10 ~ bottom)

      "transform-origin:left 10px"
        `isRenderedFrom`
          (transformOrigin := left ~ px 10)

      "transform-origin:right 20%"
        `isRenderedFrom`
          (transformOrigin := right ~ pct 20)

      "transform-origin:left 20% 10px"
        `isRenderedFrom`
          (transformOrigin := left ~ pct 20 ~ px 10)

      "transform-origin:10px top 20px"
        `isRenderedFrom`
          (transformOrigin := px 10 ~ top ~ px 20)

      "transform-origin:10px 20% 10px"
        `isRenderedFrom`
          (transformOrigin := px 10 ~ pct 20 ~ px 10)

      "transform-origin:20% 10px 20px"
        `isRenderedFrom`
          (transformOrigin := pct 20 ~ px 10 ~ px 20)
