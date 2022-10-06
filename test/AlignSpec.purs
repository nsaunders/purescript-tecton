-- https://www.w3.org/TR/css-align-3/

module Test.AlignSpec where

import Prelude

import Tecton (columnGap, inherit, gap, initial, normal, pct, px, rowGap, unset, (:=), (~))
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFrom)

spec :: Spec Unit
spec =
  describe "Box Alignment Module" do

    describe "row-gap property" do

      "row-gap:inherit" `isRenderedFrom` (rowGap := inherit)

      "row-gap:initial" `isRenderedFrom` (rowGap := initial)

      "row-gap:unset" `isRenderedFrom` (rowGap := unset)

      "row-gap:normal" `isRenderedFrom` (rowGap := normal)

      "row-gap:4px" `isRenderedFrom` (rowGap := px 4)

      "row-gap:10%" `isRenderedFrom` (rowGap := pct 10)

    describe "column-gap property" do

      "column-gap:inherit" `isRenderedFrom` (columnGap := inherit)

      "column-gap:initial" `isRenderedFrom` (columnGap := initial)

      "column-gap:unset" `isRenderedFrom` (columnGap := unset)

      "column-gap:normal" `isRenderedFrom` (columnGap := normal)

      "column-gap:4px" `isRenderedFrom` (columnGap := px 4)

      "column-gap:10%" `isRenderedFrom` (columnGap := pct 10)

    describe "gap property" do

      "gap:inherit" `isRenderedFrom` (gap := inherit)

      "gap:initial" `isRenderedFrom` (gap := initial)

      "gap:unset" `isRenderedFrom` (gap := unset)

      "gap:normal" `isRenderedFrom` (gap := normal)

      "gap:10px" `isRenderedFrom` (gap := px 10)

      "gap:1%" `isRenderedFrom` (gap := pct 1)

      "gap:normal normal" `isRenderedFrom` (gap := normal ~ normal)

      "gap:normal 1px" `isRenderedFrom` (gap := normal ~ px 1)

      "gap:normal 10%" `isRenderedFrom` (gap := normal ~ pct 10)

      "gap:1px normal" `isRenderedFrom` (gap := px 1 ~ normal)

      "gap:10% normal" `isRenderedFrom` (gap := pct 10 ~ normal)
