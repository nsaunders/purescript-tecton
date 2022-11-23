-- https://www.w3.org/TR/css-align-3/

module Test.AlignSpec where

import Prelude

import Tecton (alignSelf, auto, baseline, center, columnGap, end, first, flexEnd, flexStart, gap, initial, inherit, justifyItems, justifySelf, last, left, legacy, normal, pct, px, right, rowGap, safe, selfEnd, selfStart, start, stretch, unsafe, unset, (:=), (~))
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

    describe "align-self property" do

      "align-self:inherit" `isRenderedFrom` (alignSelf := inherit)

      "align-self:initial" `isRenderedFrom` (alignSelf := initial)

      "align-self:unset" `isRenderedFrom` (alignSelf := unset)

      "align-self:auto" `isRenderedFrom` (alignSelf := auto)

      "align-self:normal" `isRenderedFrom` (alignSelf := normal)

      "align-self:stretch" `isRenderedFrom` (alignSelf := stretch)

      "align-self:baseline" `isRenderedFrom` (alignSelf := baseline)

      "align-self:first baseline"
        `isRenderedFrom`
        (alignSelf := first ~ baseline)

      "align-self:last baseline" `isRenderedFrom` (alignSelf := last ~ baseline)

      "align-self:center" `isRenderedFrom` (alignSelf := center)

      "align-self:start" `isRenderedFrom` (alignSelf := start)

      "align-self:end" `isRenderedFrom` (alignSelf := end)

      "align-self:self-start" `isRenderedFrom` (alignSelf := selfStart)

      "align-self:self-end" `isRenderedFrom` (alignSelf :=  selfEnd)

      "align-self:flex-start" `isRenderedFrom` (alignSelf :=  flexStart)

      "align-self:flex-end" `isRenderedFrom` (alignSelf := flexEnd)

      "align-self:safe center" `isRenderedFrom` (alignSelf := safe ~ center)

      "align-self:safe start" `isRenderedFrom` (alignSelf := safe ~ start)

      "align-self:safe end" `isRenderedFrom` (alignSelf := safe ~ end)

      "align-self:safe self-start"
        `isRenderedFrom`
        (alignSelf := safe ~ selfStart)

      "align-self:safe self-end" `isRenderedFrom` (alignSelf := safe ~ selfEnd)

      "align-self:safe flex-start"
        `isRenderedFrom`
        (alignSelf := safe ~ flexStart)

      "align-self:unsafe flex-end" `isRenderedFrom` (alignSelf := unsafe ~ flexEnd)

      "align-self:unsafe center" `isRenderedFrom` (alignSelf := unsafe ~ center)

      "align-self:unsafe start" `isRenderedFrom` (alignSelf := unsafe ~ start)

      "align-self:unsafe end" `isRenderedFrom` (alignSelf := unsafe ~ end)

      "align-self:unsafe self-start"
        `isRenderedFrom`
        (alignSelf := unsafe ~ selfStart)

      "align-self:unsafe self-end" `isRenderedFrom` (alignSelf := unsafe ~ selfEnd)

      "align-self:unsafe flex-start"
        `isRenderedFrom`
        (alignSelf := unsafe ~ flexStart)

      "align-self:unsafe flex-end" `isRenderedFrom` (alignSelf := unsafe ~ flexEnd)

    describe "justify-items property" do

      "justify-items:inherit" `isRenderedFrom` (justifyItems := inherit)

      "justify-items:initial" `isRenderedFrom` (justifyItems := initial)

      "justify-items:unset" `isRenderedFrom` (justifyItems := unset)

      "justify-items:normal" `isRenderedFrom` (justifyItems := normal)

      "justify-items:stretch" `isRenderedFrom` (justifyItems := stretch)

      "justify-items:baseline" `isRenderedFrom` (justifyItems := baseline)

      "justify-items:first baseline"
        `isRenderedFrom`
        (justifyItems := first ~ baseline)

      "justify-items:last baseline"
        `isRenderedFrom`
        (justifyItems := last ~ baseline)

      "justify-items:center" `isRenderedFrom` (justifyItems := center)

      "justify-items:start" `isRenderedFrom` (justifyItems := start)

      "justify-items:end" `isRenderedFrom` (justifyItems := end)

      "justify-items:self-start" `isRenderedFrom` (justifyItems := selfStart)

      "justify-items:self-end" `isRenderedFrom` (justifyItems := selfEnd)

      "justify-items:flex-start" `isRenderedFrom` (justifyItems := flexStart)

      "justify-items:flex-end" `isRenderedFrom` (justifyItems := flexEnd)

      "justify-items:left" `isRenderedFrom` (justifyItems := left)

      "justify-items:right" `isRenderedFrom` (justifyItems := right)

      "justify-items:safe center"
        `isRenderedFrom`
        (justifyItems := safe ~ center)

      "justify-items:safe start" `isRenderedFrom` (justifyItems := safe ~ start)

      "justify-items:safe end" `isRenderedFrom` (justifyItems := safe ~ end)

      "justify-items:safe self-start"
        `isRenderedFrom`
        (justifyItems := safe ~ selfStart)

      "justify-items:safe self-end"
        `isRenderedFrom`
        (justifyItems := safe ~ selfEnd)

      "justify-items:safe flex-start"
        `isRenderedFrom`
        (justifyItems := safe ~ flexStart)

      "justify-items:safe flex-end"
        `isRenderedFrom`
        (justifyItems := safe ~ flexEnd)

      "justify-items:safe left" `isRenderedFrom` (justifyItems := safe ~ left)

      "justify-items:safe right" `isRenderedFrom` (justifyItems := safe ~ right)

      "justify-items:unsafe center"
        `isRenderedFrom`
        (justifyItems := unsafe ~ center)

      "justify-items:unsafe start"
        `isRenderedFrom`
        (justifyItems := unsafe ~ start)

      "justify-items:unsafe end" `isRenderedFrom` (justifyItems := unsafe ~ end)

      "justify-items:unsafe self-start"
        `isRenderedFrom`
        (justifyItems := unsafe ~ selfStart)

      "justify-items:unsafe self-end"
        `isRenderedFrom`
        (justifyItems := unsafe ~ selfEnd)

      "justify-items:unsafe flex-start"
        `isRenderedFrom`
        (justifyItems := unsafe ~ flexStart)

      "justify-items:unsafe flex-end"
        `isRenderedFrom`
        (justifyItems := unsafe ~ flexEnd)

      "justify-items:unsafe left" `isRenderedFrom` (justifyItems := unsafe ~ left)

      "justify-items:unsafe right"
        `isRenderedFrom`
        (justifyItems := unsafe ~ right)

      "justify-items:legacy" `isRenderedFrom` (justifyItems := legacy)

      "justify-items:legacy left"
        `isRenderedFrom`
        (justifyItems := legacy ~ left)

      "justify-items:legacy right"
        `isRenderedFrom`
        (justifyItems := legacy ~ right)

      "justify-items:legacy center"
        `isRenderedFrom`
        (justifyItems := legacy ~ center)
