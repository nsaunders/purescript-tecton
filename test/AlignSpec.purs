-- https://www.w3.org/TR/css-align-3/

module Test.AlignSpec where

import Prelude

import Tecton (auto, baseline, center, columnGap, end, first, flexEnd, flexStart, inherit, gap, initial, justifySelf, last, left, normal, pct, px, right, rowGap, safe, selfEnd, selfStart, start, stretch, unsafe, unset, (:=), (~))
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFromInline)

spec :: Spec Unit
spec = do

  let isRenderedFrom = isRenderedFromInline

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

    describe "justify-self property" do

      "justify-self:inherit" `isRenderedFrom` (justifySelf := inherit)

      "justify-self:initial" `isRenderedFrom` (justifySelf := initial)

      "justify-self:unset" `isRenderedFrom` (justifySelf := unset)

      "justify-self:auto" `isRenderedFrom` (justifySelf := auto)

      "justify-self:normal" `isRenderedFrom` (justifySelf := normal)

      "justify-self:stretch" `isRenderedFrom` (justifySelf := stretch)

      "justify-self:baseline" `isRenderedFrom` (justifySelf := baseline)

      "justify-self:first baseline"
        `isRenderedFrom`
        (justifySelf := first ~ baseline)

      "justify-self:last baseline"
        `isRenderedFrom`
        (justifySelf := last ~ baseline)

      "justify-self:center" `isRenderedFrom` (justifySelf := center)

      "justify-self:start" `isRenderedFrom` (justifySelf := start)

      "justify-self:end" `isRenderedFrom` (justifySelf := end)

      "justify-self:self-start" `isRenderedFrom` (justifySelf := selfStart)

      "justify-self:self-end" `isRenderedFrom` (justifySelf := selfEnd)

      "justify-self:flex-start" `isRenderedFrom` (justifySelf := flexStart)

      "justify-self:flex-end" `isRenderedFrom` (justifySelf := flexEnd)

      "justify-self:left" `isRenderedFrom` (justifySelf := left)

      "justify-self:right" `isRenderedFrom` (justifySelf := right)

      "justify-self:safe center" `isRenderedFrom` (justifySelf := safe ~ center)

      "justify-self:safe start" `isRenderedFrom` (justifySelf := safe ~ start)

      "justify-self:safe end" `isRenderedFrom` (justifySelf := safe ~ end)

      "justify-self:safe self-start"
        `isRenderedFrom`
        (justifySelf := safe ~ selfStart)

      "justify-self:safe self-end"
        `isRenderedFrom`
        (justifySelf := safe ~ selfEnd)

      "justify-self:safe flex-start"
        `isRenderedFrom`
        (justifySelf := safe ~ flexStart)

      "justify-self:safe flex-end"
        `isRenderedFrom`
        (justifySelf := safe ~ flexEnd)

      "justify-self:safe left" `isRenderedFrom` (justifySelf := safe ~ left)

      "justify-self:safe right" `isRenderedFrom` (justifySelf := safe ~ right)

      "justify-self:unsafe center"
        `isRenderedFrom`
        (justifySelf := unsafe ~ center)

      "justify-self:unsafe start"
        `isRenderedFrom`
        (justifySelf := unsafe ~ start)

      "justify-self:unsafe end" `isRenderedFrom` (justifySelf := unsafe ~ end)

      "justify-self:unsafe self-start"
        `isRenderedFrom`
        (justifySelf := unsafe ~ selfStart)

      "justify-self:unsafe self-end"
        `isRenderedFrom`
        (justifySelf := unsafe ~ selfEnd)

      "justify-self:unsafe flex-start"
        `isRenderedFrom`
        (justifySelf := unsafe ~ flexStart)

      "justify-self:unsafe flex-end"
        `isRenderedFrom`
        (justifySelf := unsafe ~ flexEnd)

      "justify-self:unsafe left" `isRenderedFrom` (justifySelf := unsafe ~ left)

      "justify-self:unsafe right"
        `isRenderedFrom`
        (justifySelf := unsafe ~ right)
