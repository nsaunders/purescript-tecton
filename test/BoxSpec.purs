-- https://www.w3.org/TR/css-box-3/

module Test.BoxSpec where

import Prelude

import Data.Tuple.Nested ((/\))
import PSCSS (auto, em, inherit, initial, pct, px, unset)
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

      "margin-top:auto" `isRenderedFrom` { marginTop: auto }

    describe "margin-right property" do

      "margin-right:inherit" `isRenderedFrom` { marginRight: inherit }

      "margin-right:initial" `isRenderedFrom` { marginRight: initial }

      "margin-right:unset" `isRenderedFrom` { marginRight: unset }

      "margin-right:1px" `isRenderedFrom` { marginRight: px 1 }

      "margin-right:10%" `isRenderedFrom` { marginRight: pct 10 }

      "margin-right:auto" `isRenderedFrom` { marginRight: auto }

    describe "margin-bottom property" do

      "margin-bottom:inherit" `isRenderedFrom` { marginBottom: inherit }

      "margin-bottom:initial" `isRenderedFrom` { marginBottom: initial }

      "margin-bottom:unset" `isRenderedFrom` { marginBottom: unset }

      "margin-bottom:1px" `isRenderedFrom` { marginBottom: px 1 }

      "margin-bottom:10%" `isRenderedFrom` { marginBottom: pct 10 }

      "margin-bottom:auto" `isRenderedFrom` { marginBottom: auto }

    describe "margin-left property" do

      "margin-left:inherit" `isRenderedFrom` { marginLeft: inherit }

      "margin-left:initial" `isRenderedFrom` { marginLeft: initial }

      "margin-left:unset" `isRenderedFrom` { marginLeft: unset }

      "margin-left:1px" `isRenderedFrom` { marginLeft: px 1 }

      "margin-left:10%" `isRenderedFrom` { marginLeft: pct 10 }

      "margin-left:auto" `isRenderedFrom` { marginLeft: auto }

    describe "margin property" do

      "margin:inherit" `isRenderedFrom` { margin: inherit }

      "margin:initial" `isRenderedFrom` { margin: initial }

      "margin:unset" `isRenderedFrom` { margin: unset }

      "margin:1px" `isRenderedFrom` { margin: px 1 }

      "margin:10%" `isRenderedFrom` { margin: pct 10 }

      "margin:1px 10%" `isRenderedFrom` { margin: px 1 /\ pct 10 }

      "margin:10% 1px 25%" `isRenderedFrom` { margin: pct 10 /\ px 1 /\ pct 25 }

      "margin:1px 1em 25% auto"
        `isRenderedFrom`
        { margin: px 1 /\ em 1 /\ pct 25 /\ auto }
