-- https://www.w3.org/TR/css-fonts-4/

module Test.FontsSpec where

import Prelude

import Data.Tuple.Nested ((/\))
import Tecton
  ( auto
  , bold
  , bolder
  , condensed
  , cursive
  , deg
  , emoji
  , expanded
  , extraCondensed
  , extraExpanded
  , fangsong
  , fantasy
  , fontFace
  , fontFamily
  , fontSize
  , fontSizeAdjust
  , fontStretch
  , fontStyle
  , fontWeight
  , format
  , inherit
  , initial
  , italic
  , large
  , larger
  , lighter
  , local
  , math
  , medium
  , monospace
  , normal
  , oblique
  , pct
  , px
  , sansSerif
  , semiCondensed
  , semiExpanded
  , serif
  , small
  , smaller
  , src
  , svg
  , systemUI
  , uiMonospace
  , uiRounded
  , uiSansSerif
  , uiSerif
  , ultraCondensed
  , ultraExpanded
  , unset
  , url
  , woff
  , xLarge
  , xSmall
  , xxLarge
  , xxSmall
  , (:=)
  , (?)
  , (~)
  )
import Tecton.Rule as Rule
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFromInline, isRenderedFromSheet)

spec :: Spec Unit
spec =
  describe "Fonts Module" do

    describe "font-family property" do

      let isRenderedFrom = isRenderedFromInline

      "font-family:inherit" `isRenderedFrom` (fontFamily := inherit)

      "font-family:initial" `isRenderedFrom` (fontFamily := initial)

      "font-family:unset" `isRenderedFrom` (fontFamily := unset)

      "font-family:serif" `isRenderedFrom` (fontFamily := serif)

      "font-family:sans-serif" `isRenderedFrom` (fontFamily := sansSerif)

      "font-family:cursive" `isRenderedFrom` (fontFamily := cursive)

      "font-family:fantasy" `isRenderedFrom` (fontFamily := fantasy)

      "font-family:monospace" `isRenderedFrom` (fontFamily := monospace)

      "font-family:system-ui" `isRenderedFrom` (fontFamily := systemUI)

      "font-family:emoji" `isRenderedFrom` (fontFamily := emoji)

      "font-family:math" `isRenderedFrom` (fontFamily := math)

      "font-family:fangsong" `isRenderedFrom` (fontFamily := fangsong)

      "font-family:ui-serif" `isRenderedFrom` (fontFamily := uiSerif)

      "font-family:ui-sans-serif" `isRenderedFrom` (fontFamily := uiSansSerif)

      "font-family:ui-monospace" `isRenderedFrom` (fontFamily := uiMonospace)

      "font-family:ui-rounded" `isRenderedFrom` (fontFamily := uiRounded)

      "font-family:\"Roboto\",\"Noto Sans\",sans-serif"
        `isRenderedFrom`
          (fontFamily := "Roboto" /\ "Noto Sans" /\ sansSerif)

    describe "font-weight property" do

      let isRenderedFrom = isRenderedFromInline

      "font-weight:inherit" `isRenderedFrom` (fontWeight := inherit)

      "font-weight:initial" `isRenderedFrom` (fontWeight := initial)

      "font-weight:unset" `isRenderedFrom` (fontWeight := unset)

      "font-weight:400" `isRenderedFrom` (fontWeight := 400)

      "font-weight:700" `isRenderedFrom` (fontWeight := 700)

      "font-weight:normal" `isRenderedFrom` (fontWeight := normal)

      "font-weight:bold" `isRenderedFrom` (fontWeight := bold)

      "font-weight:bolder" `isRenderedFrom` (fontWeight := bolder)

      "font-weight:lighter" `isRenderedFrom` (fontWeight := lighter)

    describe "font-stretch property" do

      let isRenderedFrom = isRenderedFromInline

      "font-stretch:inherit" `isRenderedFrom` (fontStretch := inherit)

      "font-stretch:initial" `isRenderedFrom` (fontStretch := initial)

      "font-stretch:unset" `isRenderedFrom` (fontStretch := unset)

      "font-stretch:normal" `isRenderedFrom` (fontStretch := normal)

      "font-stretch:50%" `isRenderedFrom` (fontStretch := pct 50)

      "font-stretch:ultra-condensed"
        `isRenderedFrom`
          (fontStretch := ultraCondensed)

      "font-stretch:extra-condensed"
        `isRenderedFrom`
          (fontStretch := extraCondensed)

      "font-stretch:condensed" `isRenderedFrom` (fontStretch := condensed)

      "font-stretch:semi-condensed"
        `isRenderedFrom`
          (fontStretch := semiCondensed)

      "font-stretch:semi-expanded"
        `isRenderedFrom`
          (fontStretch := semiExpanded)

      "font-stretch:expanded" `isRenderedFrom` (fontStretch := expanded)

      "font-stretch:extra-expanded"
        `isRenderedFrom`
          (fontStretch := extraExpanded)

      "font-stretch:ultra-expanded"
        `isRenderedFrom`
          (fontStretch := ultraExpanded)

    describe "font-style property" do

      let isRenderedFrom = isRenderedFromInline

      "font-style:inherit" `isRenderedFrom` (fontStyle := inherit)

      "font-style:initial" `isRenderedFrom` (fontStyle := initial)

      "font-style:unset" `isRenderedFrom` (fontStyle := unset)

      "font-style:normal" `isRenderedFrom` (fontStyle := normal)

      "font-style:italic" `isRenderedFrom` (fontStyle := italic)

      "font-style:oblique" `isRenderedFrom` (fontStyle := oblique)

      "font-style:oblique 7deg" `isRenderedFrom` (fontStyle := oblique ~ deg 7)

    describe "font-size property" do

      let isRenderedFrom = isRenderedFromInline

      "font-size:inherit" `isRenderedFrom` (fontSize := inherit)

      "font-size:initial" `isRenderedFrom` (fontSize := initial)

      "font-size:unset" `isRenderedFrom` (fontSize := unset)

      "font-size:xx-small" `isRenderedFrom` (fontSize := xxSmall)

      "font-size:x-small" `isRenderedFrom` (fontSize := xSmall)

      "font-size:small" `isRenderedFrom` (fontSize := small)

      "font-size:medium" `isRenderedFrom` (fontSize := medium)

      "font-size:large" `isRenderedFrom` (fontSize := large)

      "font-size:x-large" `isRenderedFrom` (fontSize := xLarge)

      "font-size:xx-large" `isRenderedFrom` (fontSize := xxLarge)

      "font-size:larger" `isRenderedFrom` (fontSize := larger)

      "font-size:smaller" `isRenderedFrom` (fontSize := smaller)

      "font-size:14px" `isRenderedFrom` (fontSize := px 14)

      "font-size:80%" `isRenderedFrom` (fontSize := pct 80)

    describe "font-size-adjust property" do

      let isRenderedFrom = isRenderedFromInline

      "font-size-adjust:inherit" `isRenderedFrom` (fontSizeAdjust := inherit)

      "font-size-adjust:initial" `isRenderedFrom` (fontSizeAdjust := initial)

      "font-size-adjust:unset" `isRenderedFrom` (fontSizeAdjust := unset)

      "font-size-adjust:0.5" `isRenderedFrom` (fontSizeAdjust := 0.5)

    describe "font-face rule" do

      let isRenderedFrom = isRenderedFromSheet

      "@font-face{font-family:\"Foo\";src:url(\"./foo.woff\")}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := url "./foo.woff"

    describe "font-family descriptor" do

      let isRenderedFrom = isRenderedFromSheet

      "@font-face{font-family:\"Roboto\";src:url(\"./foo.woff\")}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Roboto"
            src := url "./foo.woff"

    describe "src descriptor" do

      let isRenderedFrom = isRenderedFromSheet

      "@font-face{font-family:\"Foo\";src:url(\"./Gentium.svg\") format(\"svg\")}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := url "./Gentium.svg" ~ format svg

      "@font-face{font-family:\"Foo\";src:local(\"Gentium\"),url(\"./Gentium.woff\") format(\"woff\")}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Gentium" /\ url "./Gentium.woff" ~ format woff

    describe "font-style descriptor" do

      let isRenderedFrom = isRenderedFromSheet

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-style:auto}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontStyle := auto

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-style:normal}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontStyle := normal

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-style:italic}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontStyle := italic

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-style:oblique}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontStyle := oblique

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-style:oblique 15deg}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontStyle := oblique ~ deg 15

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-style:oblique 10deg 20deg}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontStyle := oblique ~ deg 10 ~ deg 20

    describe "font-weight descriptor" do

      let isRenderedFrom = isRenderedFromSheet

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-weight:auto}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontWeight := auto

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-weight:normal}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontWeight := normal

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-weight:bold}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontWeight := bold

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-weight:1}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontWeight := 1

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-weight:1000}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontWeight := 1000

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-weight:200 400}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontWeight := 200 ~ 400

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-weight:200 normal}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontWeight := 200 ~ normal

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-weight:500 bold}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontWeight := 500 ~ bold

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-weight:normal 600}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontWeight := normal ~ 600

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-weight:bold 900}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontWeight := bold ~ 900

    describe "font-stretch descriptor" do

      let isRenderedFrom = isRenderedFromSheet

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-stretch:auto}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontStretch := auto

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-stretch:normal}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontStretch := normal

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-stretch:110%}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontStretch := pct 110

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-stretch:ultra-condensed}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontStretch := ultraCondensed

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-stretch:extra-condensed}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontStretch := extraCondensed

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-stretch:condensed}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontStretch := condensed

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-stretch:semi-condensed}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontStretch := semiCondensed

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-stretch:semi-expanded}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontStretch := semiExpanded

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-stretch:expanded}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontStretch := expanded

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-stretch:extra-expanded}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontStretch := extraExpanded

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-stretch:ultra-expanded}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontStretch := ultraExpanded

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-stretch:normal expanded}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontStretch := normal ~ expanded

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-stretch:expanded ultra-expanded}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontStretch := expanded ~ ultraExpanded

      "@font-face{font-family:\"Foo\";src:local(\"Foo\");font-stretch:condensed 110%}"
        `isRenderedFrom` do
          fontFace ? Rule.do
            fontFamily := "Foo"
            src := local "Foo"
            fontStretch := condensed ~ pct 110
