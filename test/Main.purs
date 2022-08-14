module Test.Main where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import PSCSS (auto, noRepeat, repeat, repeatX, unset, (~))
import Test.AnimationsSpec as Animations
import Test.BackgroundsSpec as Backgrounds
import Test.BoxSpec as Box
import Test.ColorSpec as Color
import Test.ImagesSpec as Images
import Test.MediaQueriesSpec as MediaQueries
import Test.OverflowSpec as Overflow
import Test.SelectorsSpec as Selectors
import Test.SizingSpec as Sizing
import Test.Spec (describe, it)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.TextSpec as Text
import Test.TransformsSpec as Transforms
import Test.WritingModesSpec as WritingModes
import Test.Util (assertRenderedGuarded)

main :: Effect Unit
main =
  launchAff_ $
    runSpec [consoleReporter] do
      describe "Properties" do
        it "can each be guarded on a condition" do
          assertRenderedGuarded mempty "width:auto" \g -> { width: g auto }
          assertRenderedGuarded
            mempty
            "background-repeat:repeat-x,no-repeat repeat"
            \g -> { backgroundRepeat: g $ repeatX /\ noRepeat ~ repeat }
          assertRenderedGuarded
            mempty
            "text-transform:unset"
            \g -> { textTransform: g unset }
      Animations.spec
      Backgrounds.spec
      Box.spec
      Color.spec
      Images.spec
      MediaQueries.spec
      Overflow.spec
      Selectors.spec
      Sizing.spec
      Text.spec
      Transforms.spec
      WritingModes.spec
