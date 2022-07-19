-- https://www.w3.org/TR/css-animations-1/

module Test.AnimationsSpec where

import Prelude

import Data.NonEmpty ((:|))
import Data.Tuple.Nested ((/\))
import PSCSS (all, inherit, initial, keyframes, keyframesName, media, ms, nil, none, pct, sec, unset, (?), (@*), (@+@), (@/))
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
