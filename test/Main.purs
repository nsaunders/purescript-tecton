module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.AnimationsSpec as Animations
import Test.BoxSpec as Box
import Test.BoxSizingSpec as BoxSizing
import Test.ColorSpec as Color
import Test.MediaQueriesSpec as MediaQueries
import Test.SelectorsSpec as Selectors
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  Animations.spec
  Box.spec
  BoxSizing.spec
  Color.spec
  MediaQueries.spec
  Selectors.spec
