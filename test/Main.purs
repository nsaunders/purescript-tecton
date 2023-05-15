module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.AlignSpec as Align
import Test.AnimationsSpec as Animations
import Test.BackgroundsSpec as Backgrounds
import Test.BoxSpec as Box
import Test.ColorSpec as Color
import Test.ContentSpec as Content
import Test.DisplaySpec as Display
import Test.FlexboxSpec as Flexbox
import Test.FontsSpec as Fonts
import Test.GridSpec as Grid
import Test.ImagesSpec as Images
import Test.InlineSpec as Inline
import Test.ListsSpec as Lists
import Test.MaskingSpec as Masking
import Test.MediaQueriesSpec as MediaQueries
import Test.OverflowSpec as Overflow
import Test.PositionSpec as Position
import Test.RenderSpec as Render
import Test.SelectorsSpec as Selectors
import Test.SizingSpec as Sizing
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.TextDecorSpec as TextDecor
import Test.TextSpec as Text
import Test.TransformsSpec as Transforms
import Test.TransitionsSpec as Transitions
import Test.UISpec as UI
import Test.UnsafeDeclarationSpec as UnsafeDeclaration
import Test.VisufxSpec as Visufx
import Test.VisurenSpec as Visuren
import Test.WritingModesSpec as WritingModes

main :: Effect Unit
main =
  launchAff_ $
    runSpec [ consoleReporter ] do
      Align.spec
      Animations.spec
      Backgrounds.spec
      Box.spec
      Color.spec
      Content.spec
      Display.spec
      Flexbox.spec
      Fonts.spec
      Grid.spec
      Images.spec
      Inline.spec
      Lists.spec
      Masking.spec
      MediaQueries.spec
      Overflow.spec
      Position.spec
      Render.spec
      Selectors.spec
      Sizing.spec
      Text.spec
      TextDecor.spec
      Transforms.spec
      Transitions.spec
      UI.spec
      UnsafeDeclaration.spec
      Visufx.spec
      Visuren.spec
      WritingModes.spec
