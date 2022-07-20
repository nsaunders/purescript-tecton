-- https://www.w3.org/TR/css-animations-1/

module Test.AnimationsSpec where

import Prelude

import Data.NonEmpty ((:|))
import Data.Tuple.Nested ((/\))
import PSCSS (all, alternate, alternateReverse, cubicBezier, ease, easeIn, easeInOut, easeOut, end, infinite, inherit, initial, jumpBoth, jumpEnd, jumpNone, jumpStart, keyframes, keyframesName, linear, media, ms, nil, none, normal, paused, pct, reverse, running, sec, start, stepEnd, stepStart, steps, unset, (?), (@*), (@+@), (@/))
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFrom)

spec :: Spec Unit
spec =
  describe "Animations Module" do

    describe "Keyframes" do

      "@keyframes foo{0%{}100%{}}"
        `isRenderedFrom` do
        keyframes (keyframesName "foo") ? do
          pct 0 ? {}
          pct 100 ? {}

      "@media all{@keyframes foo{0%,100%{}}}"
        `isRenderedFrom` do
        media all {} ?
          keyframes (keyframesName "foo") ?
            (pct 0 :| [pct 100]) ? {}

      "@keyframes foo{0%{width:75%}20%{width:80%}50%{width:100%}}"
        `isRenderedFrom` do
        keyframes (keyframesName "foo") ? do
          pct 0 ? { width: pct 75 }
          pct 10 @+@ pct 5 @* 2 ? { width: pct 80 }
          pct 100 @/ 2 ? { width: pct 100 }

    describe "animation-name property" do

      "animation-name:inherit" `isRenderedFrom` { animationName: inherit }

      "animation-name:initial" `isRenderedFrom` { animationName: initial }

      "animation-name:unset" `isRenderedFrom` { animationName: unset }

      "animation-name:none" `isRenderedFrom` { animationName: none }

      "animation-name:xx" `isRenderedFrom` { animationName: keyframesName "xx" }

      "animation-name:foo,none,bar"
        `isRenderedFrom`
        { animationName: keyframesName "foo" /\ none /\ keyframesName "bar" }

    describe "animation-duration property" do

      "animation-duration:inherit"
        `isRenderedFrom`
        { animationDuration: inherit }

      "animation-duration:initial"
        `isRenderedFrom`
        { animationDuration: initial }

      "animation-duration:unset" `isRenderedFrom` { animationDuration: unset }

      "animation-duration:150ms" `isRenderedFrom` { animationDuration: ms 150 }

      "animation-duration:150ms,0,2s"
        `isRenderedFrom`
        { animationDuration: ms 150 /\ nil /\ sec 2 }

    describe "animation-timing-function property" do

      "animation-timing-function:inherit"
        `isRenderedFrom`
        { animationTimingFunction: inherit }

      "animation-timing-function:initial"
        `isRenderedFrom`
        { animationTimingFunction: initial }

      "animation-timing-function:unset"
        `isRenderedFrom`
        { animationTimingFunction: unset }

      "animation-timing-function:linear"
        `isRenderedFrom`
        { animationTimingFunction: linear }

      "animation-timing-function:ease"
        `isRenderedFrom`
        { animationTimingFunction: ease }

      "animation-timing-function:ease-in"
        `isRenderedFrom`
        { animationTimingFunction: easeIn }

      "animation-timing-function:ease-out"
        `isRenderedFrom`
        { animationTimingFunction: easeOut }

      "animation-timing-function:ease-in-out"
        `isRenderedFrom`
        { animationTimingFunction: easeInOut }

      "animation-timing-function:cubic-bezier(0.25,50,0.5,100)"
        `isRenderedFrom`
        { animationTimingFunction: cubicBezier 0.25 50 0.5 100 }

      "animation-timing-function:steps(0,jump-start)"
        `isRenderedFrom`
        { animationTimingFunction: steps 0 jumpStart }

      "animation-timing-function:steps(1,jump-end)"
        `isRenderedFrom`
        { animationTimingFunction: steps 1 jumpEnd }

      "animation-timing-function:steps(2,jump-none)"
        `isRenderedFrom`
        { animationTimingFunction: steps 2 jumpNone }

      "animation-timing-function:steps(3,jump-both)"
        `isRenderedFrom`
        { animationTimingFunction: steps 3 jumpBoth }

      "animation-timing-function:steps(4,start)"
        `isRenderedFrom`
        { animationTimingFunction: steps 4 start }

      "animation-timing-function:steps(4,end)"
        `isRenderedFrom`
        { animationTimingFunction: steps 4 end }

      "animation-timing-function:step-start"
        `isRenderedFrom`
        { animationTimingFunction: stepStart }

      "animation-timing-function:step-end"
        `isRenderedFrom`
        { animationTimingFunction: stepEnd }

      "animation-timing-function:ease,step-start,cubic-bezier(0.1,0.7,1,0.1)"
        `isRenderedFrom`
        { animationTimingFunction:
            ease /\ stepStart /\ cubicBezier 0.1 0.7 1.0 0.1
        }

    describe "animation-iteration-count property" do

      "animation-iteration-count:infinite"
        `isRenderedFrom`
        { animationIterationCount: infinite }

      "animation-iteration-count:3"
        `isRenderedFrom`
        { animationIterationCount: 3 }

      "animation-iteration-count:3,infinite,2"
        `isRenderedFrom`
        { animationIterationCount: 3 /\ infinite /\ 2 }

    describe "animation-direction property" do

      "animation-direction:inherit"
        `isRenderedFrom`
        { animationDirection: inherit }

      "animation-direction:initial"
        `isRenderedFrom`
        { animationDirection: initial }

      "animation-direction:unset" `isRenderedFrom` { animationDirection: unset }

      "animation-direction:normal"
        `isRenderedFrom`
        { animationDirection: normal }

      "animation-direction:reverse"
        `isRenderedFrom`
        { animationDirection: reverse }

      "animation-direction:alternate"
        `isRenderedFrom`
        { animationDirection: alternate }

      "animation-direction:alternate-reverse"
        `isRenderedFrom`
        { animationDirection: alternateReverse }

      "animation-direction:normal,alternate-reverse,alternate"
        `isRenderedFrom`
        { animationDirection: normal /\ alternateReverse /\ alternate }

    describe "animation-play-state property" do

      "animation-play-state:inherit"
        `isRenderedFrom`
        { animationPlayState: inherit }

      "animation-play-state:initial"
        `isRenderedFrom`
        { animationPlayState: initial }

      "animation-play-state:unset"
        `isRenderedFrom`
        { animationPlayState: unset }

      "animation-play-state:running"
        `isRenderedFrom`
        { animationPlayState: running }

      "animation-play-state:paused"
        `isRenderedFrom`
        { animationPlayState: paused }

      "animation-play-state:paused,running,running"
        `isRenderedFrom`
        { animationPlayState: paused /\ running /\ running }
