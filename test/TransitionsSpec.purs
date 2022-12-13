-- https://www.w3.org/TR/css-transitions-1/

module Test.TransitionsSpec where

import Prelude hiding (bottom, top)

import Data.Tuple.Nested ((/\))
import Tecton
  ( all
  , bottom
  , cubicBezier
  , ease
  , easeIn
  , easeInOut
  , easeOut
  , end
  , flex
  , height
  , inherit
  , initial
  , inset
  , jumpBoth
  , jumpEnd
  , jumpNone
  , jumpStart
  , left
  , marginTop
  , ms
  , nil
  , none
  , right
  , sec
  , start
  , stepEnd
  , stepStart
  , steps
  , top
  , transitionDelay
  , transitionDuration
  , transitionProperty
  , transitionTimingFunction
  , unset
  , width
  , (:=)
  )
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFromInline)

spec :: Spec Unit
spec = do

  let isRenderedFrom = isRenderedFromInline

  describe "Transitions" do

    describe "transition-property property" do

      "transition-property:inherit"
        `isRenderedFrom`
          (transitionProperty := inherit)

      "transition-property:initial"
        `isRenderedFrom`
          (transitionProperty := initial)

      "transition-property:unset" `isRenderedFrom` (transitionProperty := unset)

      "transition-property:none" `isRenderedFrom` (transitionProperty := none)

      "transition-property:all" `isRenderedFrom` (transitionProperty := all)

      "transition-property:margin-top"
        `isRenderedFrom`
          (transitionProperty := marginTop)

      "transition-property:width,height"
        `isRenderedFrom`
          (transitionProperty := width /\ height)

      "transition-property:top,right,bottom,left"
        `isRenderedFrom`
          (transitionProperty := top /\ right /\ bottom /\ left)

      "transition-property:inset" `isRenderedFrom` (transitionProperty := inset)

      "transition-property:flex" `isRenderedFrom` (transitionProperty := flex)

    describe "transition-duration property" do

      "transition-duration:inherit"
        `isRenderedFrom`
          (transitionDuration := inherit)

      "transition-duration:initial"
        `isRenderedFrom`
          (transitionDuration := initial)

      "transition-duration:unset"
        `isRenderedFrom`
          (transitionDuration := unset)

      "transition-duration:90ms" `isRenderedFrom` (transitionDuration := ms 90)

      "transition-duration:2ms,0,1s"
        `isRenderedFrom`
          (transitionDuration := ms 2 /\ nil /\ sec 1)

    describe "transition-timing-function" do

      "transition-timing-function:inherit"
        `isRenderedFrom`
          (transitionTimingFunction := inherit)

      "transition-timing-function:initial"
        `isRenderedFrom`
          (transitionTimingFunction := initial)

      "transition-timing-function:unset"
        `isRenderedFrom`
          (transitionTimingFunction := unset)

      "transition-timing-function:ease"
        `isRenderedFrom`
          (transitionTimingFunction := ease)

      "transition-timing-function:ease-in"
        `isRenderedFrom`
          (transitionTimingFunction := easeIn)

      "transition-timing-function:ease-out"
        `isRenderedFrom`
          (transitionTimingFunction := easeOut)

      "transition-timing-function:ease-in-out"
        `isRenderedFrom`
          (transitionTimingFunction := easeInOut)

      "transition-timing-function:step-start"
        `isRenderedFrom`
          (transitionTimingFunction := stepStart)

      "transition-timing-function:step-end"
        `isRenderedFrom`
          (transitionTimingFunction := stepEnd)

      "transition-timing-function:cubic-bezier(0.1,0.7,1,0.1)"
        `isRenderedFrom`
          (transitionTimingFunction := cubicBezier 0.1 0.7 1 0.1)

      "transition-timing-function:steps(4,jump-start)"
        `isRenderedFrom`
          (transitionTimingFunction := steps 4 jumpStart)

      "transition-timing-function:steps(10,jump-end)"
        `isRenderedFrom`
          (transitionTimingFunction := steps 10 jumpEnd)

      "transition-timing-function:steps(20,jump-none)"
        `isRenderedFrom`
          (transitionTimingFunction := steps 20 jumpNone)

      "transition-timing-function:steps(5,jump-both)"
        `isRenderedFrom`
          (transitionTimingFunction := steps 5 jumpBoth)

      "transition-timing-function:steps(6,start)"
        `isRenderedFrom`
          (transitionTimingFunction := steps 6 start)

      "transition-timing-function:steps(8,end)"
        `isRenderedFrom`
          (transitionTimingFunction := steps 8 end)

      "transition-timing-function:ease,step-start,cubic-bezier(0.1,0.7,1,0.1)"
        `isRenderedFrom`
          ( transitionTimingFunction :=
              ease /\ stepStart /\ cubicBezier 0.1 0.7 1 0.1
          )

    describe "transition-delay property" do

      "transition-delay:inherit"
        `isRenderedFrom`
          (transitionDelay := inherit)

      "transition-delay:initial"
        `isRenderedFrom`
          (transitionDelay := initial)

      "transition-delay:unset"
        `isRenderedFrom`
          (transitionDelay := unset)

      "transition-delay:90ms" `isRenderedFrom` (transitionDelay := ms 90)

      "transition-delay:2ms,0,1s"
        `isRenderedFrom`
          (transitionDelay := ms 2 /\ nil /\ sec 1)
