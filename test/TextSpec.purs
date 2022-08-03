-- https://www.w3.org/TR/css-text-3/

module Test.TextSpec where

import Prelude

import Data.Tuple.Nested ((/\))
import PSCSS (capitalize, fullSizeKana, fullWidth, inherit, initial, lowercase, none, unset, uppercase)
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFrom)

spec :: Spec Unit
spec =
  describe "Text Module" do

    describe "text-transform property" do

      "text-transform:inherit" `isRenderedFrom` { textTransform: inherit }

      "text-transform:initial" `isRenderedFrom` { textTransform: initial }

      "text-transform:unset" `isRenderedFrom` { textTransform: unset }

      "text-transform:none" `isRenderedFrom` { textTransform: none }

      "text-transform:capitalize" `isRenderedFrom` { textTransform: capitalize }

      "text-transform:uppercase" `isRenderedFrom` { textTransform: uppercase }

      "text-transform:lowercase" `isRenderedFrom` { textTransform: lowercase }

      "text-transform:full-width full-size-kana"
        `isRenderedFrom`
        { textTransform: fullWidth /\ fullSizeKana }

      "text-transform:capitalize full-width"
        `isRenderedFrom`
        { textTransform: capitalize /\ fullWidth }

      "text-transform:capitalize full-size-kana"
        `isRenderedFrom`
        { textTransform: capitalize /\ fullSizeKana }

      "text-transform:capitalize full-width full-size-kana"
        `isRenderedFrom`
        { textTransform: capitalize /\ fullWidth /\ fullSizeKana }
