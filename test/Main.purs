module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.AnimationsSpec as Animations
import Test.BackgroundsSpec as Backgrounds
import Test.BoxSpec as Box
import Test.ColorSpec as Color
import Test.ImagesSpec as Images
import Test.MediaQueriesSpec as MediaQueries
import Test.SelectorsSpec as Selectors
import Test.SizingSpec as Sizing
import Test.TextSpec as Text
import Test.TransformsSpec as Transforms
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  Animations.spec
  Backgrounds.spec
  Box.spec
  Color.spec
  Images.spec
  MediaQueries.spec
  Selectors.spec
  Sizing.spec
  Text.spec
  Transforms.spec
