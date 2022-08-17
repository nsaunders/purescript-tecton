-- https://www.w3.org/TR/CSS2/visudet.html

module Test.VisudetSpec where

import Prelude hiding (bottom, sub, top)

import PSCSS (baseline, bottom, inherit, initial, middle, pct, px, sub, super, textBottom, textTop, top, unset)
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFrom)

spec :: Spec Unit
spec =
  describe "Visual Formatting Module" do

    describe "vertical-align property" do

      "vertical-align:inherit" `isRenderedFrom` { verticalAlign: inherit }

      "vertical-align:initial" `isRenderedFrom` { verticalAlign: initial }

      "vertical-align:unset" `isRenderedFrom` { verticalAlign: unset }

      "vertical-align:baseline" `isRenderedFrom` { verticalAlign: baseline }

      "vertical-align:sub" `isRenderedFrom` { verticalAlign: sub }

      "vertical-align:super" `isRenderedFrom` { verticalAlign: super }

      "vertical-align:top" `isRenderedFrom` { verticalAlign: top }

      "vertical-align:text-top" `isRenderedFrom` { verticalAlign: textTop }

      "vertical-align:middle" `isRenderedFrom` { verticalAlign: middle }

      "vertical-align:bottom" `isRenderedFrom` { verticalAlign: bottom }

      "vertical-align:text-bottom"
        `isRenderedFrom`
        { verticalAlign: textBottom }

      "vertical-align:10%" `isRenderedFrom` { verticalAlign: pct 10 }

      "vertical-align:10px" `isRenderedFrom` { verticalAlign: px 10 }
