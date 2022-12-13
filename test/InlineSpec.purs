-- https://www.w3.org/TR/css-inline-3/

module Test.InlineSpec where

import Prelude hiding (bottom, sub, top)

import Tecton
  ( alignmentBaseline
  , alphabetic
  , auto
  , baseline
  , baselineShift
  , baselineSource
  , bottom
  , center
  , central
  , dominantBaseline
  , first
  , hanging
  , ideographic
  , inherit
  , initial
  , last
  , lineHeight
  , mathematical
  , middle
  , normal
  , pct
  , px
  , sub
  , super
  , textBottom
  , textTop
  , top
  , unset
  , verticalAlign
  , (:=)
  , (~)
  )
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFromInline)

spec :: Spec Unit
spec = do

  let isRenderedFrom = isRenderedFromInline

  describe "Inline Layout Module" do

    describe "dominant-baseline property" do

      "dominant-baseline:inherit" `isRenderedFrom` (dominantBaseline := inherit)

      "dominant-baseline:initial" `isRenderedFrom` (dominantBaseline := initial)

      "dominant-baseline:unset" `isRenderedFrom` (dominantBaseline := unset)

      "dominant-baseline:auto" `isRenderedFrom` (dominantBaseline := auto)

      "dominant-baseline:text-bottom"
        `isRenderedFrom`
          (dominantBaseline := textBottom)

      "dominant-baseline:alphabetic"
        `isRenderedFrom`
          (dominantBaseline := alphabetic)

      "dominant-baseline:ideographic"
        `isRenderedFrom`
          (dominantBaseline := ideographic)

      "dominant-baseline:middle" `isRenderedFrom` (dominantBaseline := middle)

      "dominant-baseline:central" `isRenderedFrom` (dominantBaseline := central)

      "dominant-baseline:mathematical"
        `isRenderedFrom`
          (dominantBaseline := mathematical)

      "dominant-baseline:hanging" `isRenderedFrom` (dominantBaseline := hanging)

      "dominant-baseline:text-top"
        `isRenderedFrom`
          (dominantBaseline := textTop)

    describe "vertical-align property" do

      "vertical-align:inherit" `isRenderedFrom` (verticalAlign := inherit)

      "vertical-align:initial" `isRenderedFrom` (verticalAlign := initial)

      "vertical-align:unset" `isRenderedFrom` (verticalAlign := unset)

      "vertical-align:first central sub"
        `isRenderedFrom`
          (verticalAlign := first ~ central ~ sub)

      "vertical-align:last middle super"
        `isRenderedFrom`
          (verticalAlign := last ~ middle ~ super)

      "vertical-align:first baseline bottom"
        `isRenderedFrom`
          (verticalAlign := first ~ baseline ~ bottom)

      "vertical-align:last text-bottom 10px"
        `isRenderedFrom`
          (verticalAlign := last ~ textBottom ~ px 10)

      "vertical-align:first central"
        `isRenderedFrom`
          (verticalAlign := first ~ central)

      "vertical-align:last text-top"
        `isRenderedFrom`
          (verticalAlign := last ~ textTop)

      "vertical-align:first middle"
        `isRenderedFrom`
          (verticalAlign := first ~ middle)

      "vertical-align:first baseline"
        `isRenderedFrom`
          (verticalAlign := first ~ baseline)

      "vertical-align:last baseline"
        `isRenderedFrom`
          (verticalAlign := last ~ baseline)

      "vertical-align:first sub"
        `isRenderedFrom`
          (verticalAlign := first ~ sub)

      "vertical-align:last 10%"
        `isRenderedFrom`
          (verticalAlign := last ~ pct 10)

      "vertical-align:ideographic top"
        `isRenderedFrom`
          (verticalAlign := ideographic ~ top)

      "vertical-align:mathematical super"
        `isRenderedFrom`
          (verticalAlign := mathematical ~ super)

      "vertical-align:middle center"
        `isRenderedFrom`
          (verticalAlign := middle ~ center)

      "vertical-align:baseline 1px"
        `isRenderedFrom`
          (verticalAlign := baseline ~ px 1)

      "vertical-align:first" `isRenderedFrom` (verticalAlign := first)

      "vertical-align:last" `isRenderedFrom` (verticalAlign := last)

      "vertical-align:baseline" `isRenderedFrom` (verticalAlign := baseline)

      "vertical-align:text-bottom"
        `isRenderedFrom`
          (verticalAlign := textBottom)

      "vertical-align:alphabetic" `isRenderedFrom` (verticalAlign := alphabetic)

      "vertical-align:ideographic"
        `isRenderedFrom`
          (verticalAlign := ideographic)

      "vertical-align:middle" `isRenderedFrom` (verticalAlign := middle)

      "vertical-align:central" `isRenderedFrom` (verticalAlign := central)

      "vertical-align:mathematical"
        `isRenderedFrom`
          (verticalAlign := mathematical)

      "vertical-align:text-top" `isRenderedFrom` (verticalAlign := textTop)

      "vertical-align:10px" `isRenderedFrom` (verticalAlign := px 10)

      "vertical-align:5%" `isRenderedFrom` (verticalAlign := pct 5)

      "vertical-align:sub" `isRenderedFrom` (verticalAlign := sub)

      "vertical-align:super" `isRenderedFrom` (verticalAlign := super)

      "vertical-align:top" `isRenderedFrom` (verticalAlign := top)

      "vertical-align:center" `isRenderedFrom` (verticalAlign := center)

      "vertical-align:bottom" `isRenderedFrom` (verticalAlign := bottom)

    describe "baseline-source property" do

      "baseline-source:inherit" `isRenderedFrom` (baselineSource := inherit)

      "baseline-source:initial" `isRenderedFrom` (baselineSource := initial)

      "baseline-source:unset" `isRenderedFrom` (baselineSource := unset)

      "baseline-source:auto" `isRenderedFrom` (baselineSource := auto)

      "baseline-source:first" `isRenderedFrom` (baselineSource := first)

      "baseline-source:last" `isRenderedFrom` (baselineSource := last)

    describe "alignment-baseline property" do

      "alignment-baseline:inherit"
        `isRenderedFrom`
          (alignmentBaseline := inherit)

      "alignment-baseline:initial"
        `isRenderedFrom`
          (alignmentBaseline := initial)

      "alignment-baseline:unset"
        `isRenderedFrom`
          (alignmentBaseline := unset)

      "alignment-baseline:baseline"
        `isRenderedFrom`
          (alignmentBaseline := baseline)

      "alignment-baseline:text-bottom"
        `isRenderedFrom`
          (alignmentBaseline := textBottom)

      "alignment-baseline:alphabetic"
        `isRenderedFrom`
          (alignmentBaseline := alphabetic)

      "alignment-baseline:ideographic"
        `isRenderedFrom`
          (alignmentBaseline := ideographic)

      "alignment-baseline:middle"
        `isRenderedFrom`
          (alignmentBaseline := middle)

      "alignment-baseline:central"
        `isRenderedFrom`
          (alignmentBaseline := central)

      "alignment-baseline:mathematical"
        `isRenderedFrom`
          (alignmentBaseline := mathematical)

      "alignment-baseline:text-top"
        `isRenderedFrom`
          (alignmentBaseline := textTop)

    describe "baseline-shift property" do

      "baseline-shift:inherit" `isRenderedFrom` (baselineShift := inherit)

      "baseline-shift:initial" `isRenderedFrom` (baselineShift := initial)

      "baseline-shift:unset" `isRenderedFrom` (baselineShift := unset)

      "baseline-shift:1px" `isRenderedFrom` (baselineShift := px 1)

      "baseline-shift:-5%" `isRenderedFrom` (baselineShift := pct (-5))

      "baseline-shift:sub" `isRenderedFrom` (baselineShift := sub)

      "baseline-shift:super" `isRenderedFrom` (baselineShift := super)

      "baseline-shift:top" `isRenderedFrom` (baselineShift := top)

      "baseline-shift:center" `isRenderedFrom` (baselineShift := center)

      "baseline-shift:bottom" `isRenderedFrom` (baselineShift := bottom)

    describe "line-height property" do

      "line-height:inherit" `isRenderedFrom` (lineHeight := inherit)

      "line-height:initial" `isRenderedFrom` (lineHeight := initial)

      "line-height:unset" `isRenderedFrom` (lineHeight := unset)

      "line-height:normal" `isRenderedFrom` (lineHeight := normal)

      "line-height:1" `isRenderedFrom` (lineHeight := 1)

      "line-height:1.5" `isRenderedFrom` (lineHeight := 1.5)

      "line-height:24px" `isRenderedFrom` (lineHeight := px 24)

      "line-height:125%" `isRenderedFrom` (lineHeight := pct 125)
