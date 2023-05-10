-- https://www.w3.org/TR/css-lists-3/

module Test.ListsSpec where

import Prelude

import Color (rgb)
import Data.Tuple.Nested ((/\))
import Tecton
  ( arabicIndic
  , armenian
  , bengali
  , cambodian
  , center
  , circle
  , cjkDecimal
  , cjkEarthlyBranch
  , cjkHeavenlyStem
  , decimal
  , decimalLeadingZero
  , devanagari
  , disc
  , disclosureClosed
  , disclosureOpen
  , georgian
  , gujarati
  , gurmukhi
  , hebrew
  , hiragana
  , hiraganaIroha
  , inherit
  , initial
  , inside
  , kannada
  , katakana
  , katakanaIroha
  , khmer
  , lao
  , li
  , listStyleImage
  , listStylePosition
  , listStyleType
  , lowerAlpha
  , lowerArmenian
  , lowerGreek
  , lowerLatin
  , lowerRoman
  , malayalam
  , marker
  , mongolian
  , myanmar
  , nil
  , none
  , oriya
  , outside
  , persian
  , radialGradient
  , square
  , tamil
  , telugu
  , thai
  , tibetan
  , unset
  , upperAlpha
  , upperArmenian
  , upperLatin
  , upperRoman
  , url
  , width
  , (&::)
  , (:=)
  , (?)
  )
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFromInline, isRenderedFromSheet)

spec :: Spec Unit
spec =
  describe "Lists and Counters Module" do

    describe "marker pseudo-element" do

      let isRenderedFrom = isRenderedFromSheet

      "li::marker{width:0}"
        `isRenderedFrom` do
          li &:: marker ? width := nil

    describe "list-style-image property" do

      let isRenderedFrom = isRenderedFromInline

      "list-style-image:inherit" `isRenderedFrom` (listStyleImage := inherit)

      "list-style-image:initial" `isRenderedFrom` (listStyleImage := initial)

      "list-style-image:unset" `isRenderedFrom` (listStyleImage := unset)

      "list-style-image:none" `isRenderedFrom` (listStyleImage := none)

      "list-style-image:url(\"http://example.com/ellipse.png\")"
        `isRenderedFrom`
          (listStyleImage := url "http://example.com/ellipse.png")

      "list-style-image:radial-gradient(circle at center,#0000ff,#ffff00)"
        `isRenderedFrom`
          ( listStyleImage :=
              radialGradient circle center $ rgb 0 0 255 /\ rgb 255 255 0
          )

    describe "list-style-type property" do

      let isRenderedFrom = isRenderedFromInline

      "list-style-type:inherit" `isRenderedFrom` (listStyleType := inherit)

      "list-style-type:initial" `isRenderedFrom` (listStyleType := initial)

      "list-style-type:unset" `isRenderedFrom` (listStyleType := unset)

      "list-style-type:decimal" `isRenderedFrom` (listStyleType := decimal)

      "list-style-type:decimal-leading-zero"
        `isRenderedFrom`
          (listStyleType := decimalLeadingZero)

      "list-style-type:arabic-indic"
        `isRenderedFrom`
          (listStyleType := arabicIndic)

      "list-style-type:armenian"
        `isRenderedFrom`
          (listStyleType := armenian)

      "list-style-type:upper-armenian"
        `isRenderedFrom`
          (listStyleType := upperArmenian)

      "list-style-type:lower-armenian"
        `isRenderedFrom`
          (listStyleType := lowerArmenian)

      "list-style-type:bengali" `isRenderedFrom` (listStyleType := bengali)

      "list-style-type:cambodian" `isRenderedFrom` (listStyleType := cambodian)

      "list-style-type:khmer" `isRenderedFrom` (listStyleType := khmer)

      "list-style-type:cjk-decimal"
        `isRenderedFrom`
          (listStyleType := cjkDecimal)

      "list-style-type:devanagari"
        `isRenderedFrom`
          (listStyleType := devanagari)

      "list-style-type:georgian" `isRenderedFrom` (listStyleType := georgian)

      "list-style-type:gujarati" `isRenderedFrom` (listStyleType := gujarati)

      "list-style-type:gurmukhi" `isRenderedFrom` (listStyleType := gurmukhi)

      "list-style-type:hebrew" `isRenderedFrom` (listStyleType := hebrew)

      "list-style-type:kannada" `isRenderedFrom` (listStyleType := kannada)

      "list-style-type:lao" `isRenderedFrom` (listStyleType := lao)

      "list-style-type:malayalam" `isRenderedFrom` (listStyleType := malayalam)

      "list-style-type:mongolian" `isRenderedFrom` (listStyleType := mongolian)

      "list-style-type:myanmar" `isRenderedFrom` (listStyleType := myanmar)

      "list-style-type:oriya" `isRenderedFrom` (listStyleType := oriya)

      "list-style-type:persian" `isRenderedFrom` (listStyleType := persian)

      "list-style-type:lower-roman"
        `isRenderedFrom`
          (listStyleType := lowerRoman)

      "list-style-type:upper-roman"
        `isRenderedFrom`
          (listStyleType := upperRoman)

      "list-style-type:tamil" `isRenderedFrom` (listStyleType := tamil)

      "list-style-type:telugu" `isRenderedFrom` (listStyleType := telugu)

      "list-style-type:thai" `isRenderedFrom` (listStyleType := thai)

      "list-style-type:tibetan" `isRenderedFrom` (listStyleType := tibetan)

      "list-style-type:lower-alpha"
        `isRenderedFrom`
          (listStyleType := lowerAlpha)

      "list-style-type:lower-latin"
        `isRenderedFrom`
          (listStyleType := lowerLatin)

      "list-style-type:upper-alpha"
        `isRenderedFrom`
          (listStyleType := upperAlpha)

      "list-style-type:upper-latin"
        `isRenderedFrom`
          (listStyleType := upperLatin)

      "list-style-type:lower-greek"
        `isRenderedFrom`
          (listStyleType := lowerGreek)

      "list-style-type:hiragana"
        `isRenderedFrom`
          (listStyleType := hiragana)

      "list-style-type:hiragana-iroha"
        `isRenderedFrom`
          (listStyleType := hiraganaIroha)

      "list-style-type:katakana"
        `isRenderedFrom`
          (listStyleType := katakana)

      "list-style-type:katakana-iroha"
        `isRenderedFrom`
          (listStyleType := katakanaIroha)

      "list-style-type:disc" `isRenderedFrom` (listStyleType := disc)

      "list-style-type:circle" `isRenderedFrom` (listStyleType := circle)

      "list-style-type:square" `isRenderedFrom` (listStyleType := square)

      "list-style-type:disclosure-open"
        `isRenderedFrom`
          (listStyleType := disclosureOpen)

      "list-style-type:disclosure-closed"
        `isRenderedFrom`
          (listStyleType := disclosureClosed)

      "list-style-type:cjk-earthly-branch"
        `isRenderedFrom`
          (listStyleType := cjkEarthlyBranch)

      "list-style-type:cjk-heavenly-stem"
        `isRenderedFrom`
          (listStyleType := cjkHeavenlyStem)

    describe "list-style-position property" do

      let isRenderedFrom = isRenderedFromInline

      "list-style-position:inherit"
        `isRenderedFrom`
          (listStylePosition := inherit)

      "list-style-position:initial"
        `isRenderedFrom`
          (listStylePosition := initial)

      "list-style-position:unset" `isRenderedFrom` (listStylePosition := unset)

      "list-style-position:inside"
        `isRenderedFrom`
          (listStylePosition := inside)

      "list-style-position:outside"
        `isRenderedFrom`
          (listStylePosition := outside)
