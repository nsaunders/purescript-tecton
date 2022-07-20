-- https://www.w3.org/TR/css-box-3/

module Test.BoxSpec where

import Prelude

import PSCSS (inherit, initial, pct, px, unset)
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFrom)

spec :: Spec Unit
spec =
  describe "Box Module" do

    describe "margin-top property" do

      "margin-top:inherit" `isRenderedFrom` { marginTop: inherit }

      "margin-top:initial" `isRenderedFrom` { marginTop: initial }

      "margin-top:unset" `isRenderedFrom` { marginTop: unset }

      "margin-top:1px" `isRenderedFrom` { marginTop: px 1 }

      "margin-top:10%" `isRenderedFrom` { marginTop: pct 10 }
