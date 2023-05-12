-- https://www.w3.org/TR/css-animations-1/

module Test.AnimationsSpec where

import Prelude

import Data.Tuple.Nested ((/\))
import Tecton
  ( KeyframesName(..)
  , all
  , alternate
  , alternateReverse
  , animationDelay
  , animationDirection
  , animationDuration
  , animationFillMode
  , animationIterationCount
  , animationName
  , animationPlayState
  , animationTimingFunction
  , backwards
  , both
  , cubicBezier
  , ease
  , easeIn
  , easeInOut
  , easeOut
  , end
  , forwards
  , infinite
  , inherit
  , initial
  , jumpBoth
  , jumpEnd
  , jumpNone
  , jumpStart
  , keyframes
  , linear
  , media
  , ms
  , nil
  , none
  , normal
  , paused
  , pct
  , px
  , reverse
  , running
  , sec
  , start
  , stepEnd
  , stepStart
  , steps
  , unset
  , width
  , (:=)
  , (?)
  , (@*)
  , (@+@)
  , (@/)
  )
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFromInline, isRenderedFromSheet)

spec :: Spec Unit
spec =
  describe "Animations Module" do

    describe "Keyframes" do

      let isRenderedFrom = isRenderedFromSheet

      "@keyframes foo{0%{width:0}100%{width:500px}}"
        `isRenderedFrom` do
          keyframes (KeyframesName "foo") ? do
            pct 0 ? width := nil
            pct 100 ? width := px 500

      "@media all{@keyframes foo{0%,100%{width:0}}}"
        `isRenderedFrom` do
          media all {} ? do
            keyframes (KeyframesName "foo") ? do
              pct 0 /\ pct 100 ? width := nil

      "@keyframes foo{0%{width:75%}20%{width:80%}50%{width:100%}}"
        `isRenderedFrom` do
          keyframes (KeyframesName "foo") ? do
            pct 0 ? width := pct 75
            pct 10 @+@ pct 5 @* 2 ? width := pct 80
            pct 100 @/ 2 ? width := pct 100

    describe "animation-name property" do

      let isRenderedFrom = isRenderedFromInline

      "animation-name:inherit" `isRenderedFrom` (animationName := inherit)

      "animation-name:initial" `isRenderedFrom` (animationName := initial)

      "animation-name:unset" `isRenderedFrom` (animationName := unset)

      "animation-name:none" `isRenderedFrom` (animationName := none)

      "animation-name:xx" `isRenderedFrom` (animationName := KeyframesName "xx")

      "animation-name:foo,none,bar"
        `isRenderedFrom`
          (animationName := KeyframesName "foo" /\ none /\ KeyframesName "bar")

    describe "animation-duration property" do

      let isRenderedFrom = isRenderedFromInline

      "animation-duration:inherit"
        `isRenderedFrom`
          (animationDuration := inherit)

      "animation-duration:initial"
        `isRenderedFrom`
          (animationDuration := initial)

      "animation-duration:unset" `isRenderedFrom` (animationDuration := unset)

      "animation-duration:150ms" `isRenderedFrom` (animationDuration := ms 150)

      "animation-duration:150ms,0,2s"
        `isRenderedFrom`
          (animationDuration := ms 150 /\ nil /\ sec 2)

    describe "animation-timing-function property" do

      let isRenderedFrom = isRenderedFromInline

      "animation-timing-function:inherit"
        `isRenderedFrom`
          (animationTimingFunction := inherit)

      "animation-timing-function:initial"
        `isRenderedFrom`
          (animationTimingFunction := initial)

      "animation-timing-function:unset"
        `isRenderedFrom`
          (animationTimingFunction := unset)

      "animation-timing-function:linear"
        `isRenderedFrom`
          (animationTimingFunction := linear)

      "animation-timing-function:ease"
        `isRenderedFrom`
          (animationTimingFunction := ease)

      "animation-timing-function:ease-in"
        `isRenderedFrom`
          (animationTimingFunction := easeIn)

      "animation-timing-function:ease-out"
        `isRenderedFrom`
          (animationTimingFunction := easeOut)

      "animation-timing-function:ease-in-out"
        `isRenderedFrom`
          (animationTimingFunction := easeInOut)

      "animation-timing-function:cubic-bezier(0.25,50,0.5,100)"
        `isRenderedFrom`
          (animationTimingFunction := cubicBezier 0.25 50 0.5 100)

      "animation-timing-function:steps(0,jump-start)"
        `isRenderedFrom`
          (animationTimingFunction := steps 0 jumpStart)

      "animation-timing-function:steps(1,jump-end)"
        `isRenderedFrom`
          (animationTimingFunction := steps 1 jumpEnd)

      "animation-timing-function:steps(2,jump-none)"
        `isRenderedFrom`
          (animationTimingFunction := steps 2 jumpNone)

      "animation-timing-function:steps(3,jump-both)"
        `isRenderedFrom`
          (animationTimingFunction := steps 3 jumpBoth)

      "animation-timing-function:steps(4,start)"
        `isRenderedFrom`
          (animationTimingFunction := steps 4 start)

      "animation-timing-function:steps(4,end)"
        `isRenderedFrom`
          (animationTimingFunction := steps 4 end)

      "animation-timing-function:step-start"
        `isRenderedFrom`
          (animationTimingFunction := stepStart)

      "animation-timing-function:step-end"
        `isRenderedFrom`
          (animationTimingFunction := stepEnd)

      "animation-timing-function:ease,step-start,cubic-bezier(0.1,0.7,1,0.1)"
        `isRenderedFrom`
          ( animationTimingFunction :=
              ease /\ stepStart /\ cubicBezier 0.1 0.7 1.0 0.1
          )

    describe "animation-iteration-count property" do

      let isRenderedFrom = isRenderedFromInline

      "animation-iteration-count:infinite"
        `isRenderedFrom`
          (animationIterationCount := infinite)

      "animation-iteration-count:3"
        `isRenderedFrom`
          (animationIterationCount := 3)

      "animation-iteration-count:3,infinite,2"
        `isRenderedFrom`
          (animationIterationCount := 3 /\ infinite /\ 2)

    describe "animation-direction property" do

      let isRenderedFrom = isRenderedFromInline

      "animation-direction:inherit"
        `isRenderedFrom`
          (animationDirection := inherit)

      "animation-direction:initial"
        `isRenderedFrom`
          (animationDirection := initial)

      "animation-direction:unset" `isRenderedFrom` (animationDirection := unset)

      "animation-direction:normal"
        `isRenderedFrom`
          (animationDirection := normal)

      "animation-direction:reverse"
        `isRenderedFrom`
          (animationDirection := reverse)

      "animation-direction:alternate"
        `isRenderedFrom`
          (animationDirection := alternate)

      "animation-direction:alternate-reverse"
        `isRenderedFrom`
          (animationDirection := alternateReverse)

      "animation-direction:normal,alternate-reverse,alternate"
        `isRenderedFrom`
          (animationDirection := normal /\ alternateReverse /\ alternate)

    describe "animation-play-state property" do

      let isRenderedFrom = isRenderedFromInline

      "animation-play-state:inherit"
        `isRenderedFrom`
          (animationPlayState := inherit)

      "animation-play-state:initial"
        `isRenderedFrom`
          (animationPlayState := initial)

      "animation-play-state:unset"
        `isRenderedFrom`
          (animationPlayState := unset)

      "animation-play-state:running"
        `isRenderedFrom`
          (animationPlayState := running)

      "animation-play-state:paused"
        `isRenderedFrom`
          (animationPlayState := paused)

      "animation-play-state:paused,running,running"
        `isRenderedFrom`
          (animationPlayState := paused /\ running /\ running)

    describe "animation-delay property" do

      let isRenderedFrom = isRenderedFromInline

      "animation-delay:inherit" `isRenderedFrom` (animationDelay := inherit)

      "animation-delay:initial" `isRenderedFrom` (animationDelay := initial)

      "animation-delay:unset" `isRenderedFrom` (animationDelay := unset)

      "animation-delay:150ms" `isRenderedFrom` (animationDelay := ms 150)

      "animation-delay:150ms,0,2s"
        `isRenderedFrom`
          (animationDelay := ms 150 /\ nil /\ sec 2)

    describe "animation-fill-mode property" do

      let isRenderedFrom = isRenderedFromInline

      "animation-fill-mode:inherit"
        `isRenderedFrom`
          (animationFillMode := inherit)

      "animation-fill-mode:initial"
        `isRenderedFrom`
          (animationFillMode := initial)

      "animation-fill-mode:unset" `isRenderedFrom` (animationFillMode := unset)

      "animation-fill-mode:none" `isRenderedFrom` (animationFillMode := none)

      "animation-fill-mode:forwards"
        `isRenderedFrom`
          (animationFillMode := forwards)

      "animation-fill-mode:backwards"
        `isRenderedFrom`
          (animationFillMode := backwards)

      "animation-fill-mode:both" `isRenderedFrom` (animationFillMode := both)

      "animation-fill-mode:none,backwards,both,forwards"
        `isRenderedFrom`
          (animationFillMode := none /\ backwards /\ both /\ forwards)
