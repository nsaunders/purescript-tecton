-- https://www.w3.org/TR/css-text-3/

module Test.TextSpec where

import Prelude

import Tecton
  ( breakAll
  , breakSpaces
  , breakWord
  , capitalize
  , center
  , em
  , end
  , fullSizeKana
  , fullWidth
  , inherit
  , initial
  , justify
  , justifyAll
  , keepAll
  , left
  , letterSpacing
  , lowercase
  , matchParent
  , none
  , normal
  , nowrap
  , pct
  , pre
  , preLine
  , preWrap
  , px
  , right
  , start
  , textAlign
  , textIndent
  , textTransform
  , unset
  , uppercase
  , whiteSpace
  , wordBreak
  , wordSpacing
  , (:=)
  , (~)
  )
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFromInline)

spec :: Spec Unit
spec = do

  let isRenderedFrom = isRenderedFromInline

  describe "Text Module" do

    describe "text-transform property" do

      "text-transform:inherit" `isRenderedFrom` (textTransform := inherit)

      "text-transform:initial" `isRenderedFrom` (textTransform := initial)

      "text-transform:unset" `isRenderedFrom` (textTransform := unset)

      "text-transform:none" `isRenderedFrom` (textTransform := none)

      "text-transform:capitalize" `isRenderedFrom` (textTransform := capitalize)

      "text-transform:uppercase" `isRenderedFrom` (textTransform := uppercase)

      "text-transform:lowercase" `isRenderedFrom` (textTransform := lowercase)

      "text-transform:full-width full-size-kana"
        `isRenderedFrom`
          (textTransform := fullWidth ~ fullSizeKana)

      "text-transform:capitalize full-width"
        `isRenderedFrom`
          (textTransform := capitalize ~ fullWidth)

      "text-transform:capitalize full-size-kana"
        `isRenderedFrom`
          (textTransform := capitalize ~ fullSizeKana)

      "text-transform:capitalize full-width full-size-kana"
        `isRenderedFrom`
          (textTransform := capitalize ~ fullWidth ~ fullSizeKana)

    describe "white-space property" do

      "white-space:inherit" `isRenderedFrom` (whiteSpace := inherit)

      "white-space:initial" `isRenderedFrom` (whiteSpace := initial)

      "white-space:unset" `isRenderedFrom` (whiteSpace := unset)

      "white-space:normal" `isRenderedFrom` (whiteSpace := normal)

      "white-space:pre" `isRenderedFrom` (whiteSpace := pre)

      "white-space:nowrap" `isRenderedFrom` (whiteSpace := nowrap)

      "white-space:pre-wrap" `isRenderedFrom` (whiteSpace := preWrap)

      "white-space:break-spaces" `isRenderedFrom` (whiteSpace := breakSpaces)

      "white-space:pre-line" `isRenderedFrom` (whiteSpace := preLine)

    describe "text-align property" do

      "text-align:inherit" `isRenderedFrom` (textAlign := inherit)

      "text-align:initial" `isRenderedFrom` (textAlign := initial)

      "text-align:unset" `isRenderedFrom` (textAlign := unset)

      "text-align:start" `isRenderedFrom` (textAlign := start)

      "text-align:end" `isRenderedFrom` (textAlign := end)

      "text-align:left" `isRenderedFrom` (textAlign := left)

      "text-align:right" `isRenderedFrom` (textAlign := right)

      "text-align:center" `isRenderedFrom` (textAlign := center)

      "text-align:justify" `isRenderedFrom` (textAlign := justify)

      "text-align:match-parent" `isRenderedFrom` (textAlign := matchParent)

      "text-align:justify-all" `isRenderedFrom` (textAlign := justifyAll)

    describe "word-spacing" do

      "word-spacing:inherit" `isRenderedFrom` (wordSpacing := inherit)

      "word-spacing:initial" `isRenderedFrom` (wordSpacing := initial)

      "word-spacing:unset" `isRenderedFrom` (wordSpacing := unset)

      "word-spacing:normal" `isRenderedFrom` (wordSpacing := normal)

      "word-spacing:0.025em" `isRenderedFrom` (wordSpacing := em 0.025)

    describe "letter-spacing" do

      "letter-spacing:inherit" `isRenderedFrom` (letterSpacing := inherit)

      "letter-spacing:initial" `isRenderedFrom` (letterSpacing := initial)

      "letter-spacing:unset" `isRenderedFrom` (letterSpacing := unset)

      "letter-spacing:normal" `isRenderedFrom` (letterSpacing := normal)

      "letter-spacing:0.025em" `isRenderedFrom` (letterSpacing := em 0.025)

    describe "text-indent property" do

      "text-indent:inherit" `isRenderedFrom` (textIndent := inherit)

      "text-indent:initial" `isRenderedFrom` (textIndent := initial)

      "text-indent:unset" `isRenderedFrom` (textIndent := unset)

      "text-indent:10px" `isRenderedFrom` (textIndent := px 10)

      "text-indent:2.5%" `isRenderedFrom` (textIndent := pct 2.5)

    describe "word-break property" do

      "word-break:inherit" `isRenderedFrom` (wordBreak := inherit)

      "word-break:initial" `isRenderedFrom` (wordBreak := initial)

      "word-break:unset" `isRenderedFrom` (wordBreak := unset)

      "word-break:normal" `isRenderedFrom` (wordBreak := normal)

      "word-break:keep-all" `isRenderedFrom` (wordBreak := keepAll)

      "word-break:break-all" `isRenderedFrom` (wordBreak := breakAll)

      "word-break:break-word" `isRenderedFrom` (wordBreak := breakWord)