module Example.ZenGarments where

import Prelude hiding (bottom, sub, top)

import Color (black, hsl, rgb, rgba)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Tecton
  ( KeyframesName(..)
  , a
  , abbr
  , absolute
  , acronym
  , after
  , all
  , animationDelay
  , animationDuration
  , animationFillMode
  , animationIterationCount
  , animationName
  , animationTimingFunction
  , auto
  , b
  , backgroundAttachment
  , backgroundColor
  , backgroundImage
  , backgroundPosition
  , backgroundRepeat
  , before
  , block
  , body
  , bold
  , borderStyle
  , borderTopColor
  , borderTopStyle
  , borderTopWidth
  , both
  , bottom
  , center
  , clear
  , code
  , color
  , content
  , deg
  , disc
  , display
  , dl
  , easeInOut
  , em
  , em'
  , firstChild
  , fixed
  , float
  , focus
  , fontFamily
  , fontSize
  , fontStyle
  , fontWeight
  , footer
  , form
  , forwards
  , h1
  , h2
  , h3
  , h4
  , h5
  , h6
  , height
  , hidden
  , hover
  , href
  , html
  , i
  , img
  , initial
  , inlineBlock
  , input
  , italic
  , keyframes
  , label
  , left
  , letterSpacing
  , li
  , lineHeight
  , linear
  , listStyleType
  , margin
  , marginBottom
  , marginLeft
  , marginRight
  , marginTop
  , mark
  , maskImage
  , maxWidth
  , media
  , monospace
  , nil
  , noRepeat
  , none
  , normal
  , nowrap
  , nth
  , nthChild
  , odd
  , ol
  , opacity
  , overflow
  , p
  , padding
  , paddingBottom
  , paddingLeft
  , paddingRight
  , paddingTop
  , pct
  , placeholder
  , position
  , pretty
  , px
  , relative
  , rem
  , renderSheet
  , right
  , role
  , rotate
  , sansSerif
  , screen
  , sec
  , selection
  , solid
  , strong
  , sub
  , sup
  , table
  , td
  , textAlign
  , textDecorationLine
  , textIndent
  , textTransform
  , textarea
  , title
  , top
  , transform
  , transitionDuration
  , transitionProperty
  , transitionTimingFunction
  , transparent
  , ul
  , universal
  , uppercase
  , url
  , video
  , visibility
  , visible
  , whiteSpace
  , width
  , (&.)
  , (&:)
  , (&::)
  , (&@)
  , (*=)
  , (:=)
  , (?)
  , (@=)
  , (|*)
  , (~)
  )
import Tecton.Rule as Rule
import Web.HTML.Common (ClassName(..))

main :: Effect Unit
main = log $ renderSheet pretty do

  -- Adapted from Dan Mall's "CSS Zen Garments"
  -- http://www.csszengarden.com/220/220.css

  -- Base
  universal ? Rule.do
    margin := nil
    padding := nil
  html ? Rule.do
    fontSize := pct 62.5
  body ? Rule.do
    fontFamily := "Helvetica" /\ "Arial" /\ sansSerif
    color := rgb 51 51 51
    backgroundColor := rgb 232 236 240
    backgroundImage := url "i/denim2.png"
    backgroundRepeat := noRepeat
    backgroundPosition := pct 50 ~ px 350

  -- block level

  h1 ? Rule.do
    fontFamily := "Helvetica" /\ "Arial" /\ sansSerif
    fontWeight := bold
    fontSize := rem 4.2
    marginBottom := em 0.5
  h2 ? Rule.do
    fontFamily := "Helvetica" /\ "Arial" /\ sansSerif
    fontWeight := bold
    fontSize := rem 1.8
    marginBottom := em 1
  h3 ? Rule.do
    fontFamily := "Helvetica" /\ "Arial" /\ sansSerif
    fontWeight := bold
    fontSize := rem 1.5
    marginBottom := em 1
  h4 /\ h5 /\ h6 ? Rule.do
    fontFamily := "Helvetica" /\ "Arial" /\ sansSerif
    fontWeight := bold
    fontSize := rem 1.2

  ( universal &. ClassName "wf-active" |* body
      /\ universal &. ClassName "wf-active" |* h1
      /\ universal &. ClassName "wf-active" |* h2
      /\ universal &. ClassName "wf-active" |* h3
      /\ universal &. ClassName "wf-active" |* h4
      /\ universal &. ClassName "wf-active" |* h5
      /\ universal &. ClassName "wf-active" |* h6
  ) ? Rule.do
    fontFamily := "effra" /\ sansSerif

  p /\ ul /\ dl /\ ol /\ table ? Rule.do
    fontSize := px 16
    marginBottom := em 1.5
  universal &. ClassName "wf-active" |* p ? Rule.do
    fontWeight := 300

  form ? Rule.do
    marginBottom := em 1.5

  -- inline

  label ? Rule.do
    fontSize := rem 1.6
  ( input &. ClassName "empty" &:: placeholder
      /\ textarea &. ClassName "empty" &:: placeholder
  ) ? Rule.do
    color := rgb 255 0 0
  universal &. ClassName "error" ? Rule.do
    color := rgb 255 0 0

  td ? Rule.do
    padding := em 0.25 ~ em 1
    borderTopWidth := px 1
    borderTopStyle := solid
    borderTopColor := hsl 0.0 0.0 0.6667

  em' ? Rule.do
    fontStyle := italic
  strong ? Rule.do
    fontWeight := bold

  mark ? Rule.do
    backgroundColor := initial
    fontStyle := normal

  video ? Rule.do
    width := pct 100
    height := auto

  a ? Rule.do
    textDecorationLine := none
    color := hsl 37.0 0.33 0.48
    position := relative
    transitionProperty := all
    transitionDuration := sec 0.25
    transitionTimingFunction := linear

  a &:: after ? Rule.do
    content := ""
    display := block
    backgroundColor := hsl 37.0 0.33 0.48
    height := px 2
    width := pct 100
    position := absolute
    bottom := em (-0.2)
    left := nil
    opacity := 0
    transitionProperty := all
    transitionTimingFunction := linear
    transitionDuration := sec 0.25

  a &: hover /\ a &: focus ? Rule.do
    color := black
  a &: hover &:: after /\ a &: focus &:: after ? Rule.do
    opacity := 1
    bottom := nil

  img ? Rule.do
    display := block
    margin := nil ~ px 10 ~ px 10 ~ nil
    maxWidth := pct 100

  a |* img ? Rule.do
    borderStyle := none

  i ? Rule.do
    fontStyle := normal
  b ? Rule.do
    fontWeight := normal

  abbr /\ abbr &@ title /\ acronym ? Rule.do
    fontSize := pct 75
    letterSpacing := em 0.2
    textTransform := uppercase
    borderStyle := none

  code ? Rule.do
    fontSize := rem 1.4
    lineHeight := 1
    fontFamily := "Consolas" /\ "Courier New" /\ "Courier" /\ monospace
    color := rgb 153 153 153

  sub /\ sup ? Rule.do
    lineHeight := 0

  universal &:: selection ? Rule.do
    backgroundColor := rgb 213 217 220
    color := rgb 51 51 51

  -- Global

  -- Phark Image Replacement - http://phark.typepad.com/phark/2003/08/accessible_imag.html
  universal &. ClassName "phark" ? Rule.do
    display := block
    textIndent := px (-9999)
    backgroundPosition := nil ~ nil
    backgroundRepeat := noRepeat
    backgroundColor := transparent

  -- Trimming Outline in Firefox - http://snook.ca/archives/html_and_css/trimming_long_o
  universal &. ClassName "phark-link" ? Rule.do
    overflow := hidden

  -- Easy Clearing - http://www.positioniseverything.net/easyclearing.html
  universal &. ClassName "clearfix" &:: after ? Rule.do
    content := "."
    display := block
    height := nil
    clear := both
    visibility := hidden

  universal &. ClassName "offscreen" ? Rule.do
    position := absolute
    left := px (-9999)
    display := block
  universal &. ClassName "onscreen" ? Rule.do
    left := nil

  universal &. ClassName "hide" ? Rule.do
    display := none

  universal &. ClassName "no-bullets" ? Rule.do
    listStyleType := none

  universal &. ClassName "bulleted" ? Rule.do
    listStyleType := disc

  universal &. ClassName "uppercase" ? Rule.do
    textTransform := uppercase

  universal &. ClassName "rwd-break" ? Rule.do
    display := block

  universal &. ClassName "kellum" ? Rule.do
    display := block
    textIndent := pct 100
    whiteSpace := nowrap
    overflow := hidden

  -- Modules

  universal &. ClassName "page-wrapper" ? Rule.do
    width := pct 90
    margin := nil ~ auto

  ( universal &. ClassName "main" |* h3
      /\ universal &. ClassName "preamble" |* h3
      /\ universal &. ClassName "select"
  ) ? Rule.do
    visibility := hidden
    fontSize := rem 2
    textTransform := uppercase
    letterSpacing := em 0.1
    color := hsl 224.0 0.4 0.25
  ( universal &. ClassName "wf-active" |* universal &. ClassName "main" |* h3
      /\ universal &. ClassName "wf-active"
        |* universal
        &. ClassName "preamble"
        |* h3
  ) ? Rule.do
    fontWeight := 900

  ( universal &. ClassName "main" |* h3 &:: after
      /\ universal &. ClassName "preamble" |* h3 &:: after
      /\ universal &. ClassName "select" &:: after
  ) ? Rule.do
    visibility := visible
    display := block

  universal &. ClassName "main" |* p /\ universal &. ClassName "preamble" |* p ?
    Rule.do
      fontSize := rem 2
  ( universal &. ClassName "wf-active" |* universal &. ClassName "main" |* p
      /\ universal &. ClassName "wf-active" |* universal &. ClassName "preamble"
        |* p
  ) ? Rule.do
    lineHeight := 1.4

  ( universal &. ClassName "next" |* a
      /\ universal &. ClassName "previous" |* a
      /\ universal &. ClassName "viewall" |* a
      /\ universal &. ClassName "zen-resources" |* a
      /\ universal &. ClassName "summary" |* p &: nthChild (nth 0 2) |* a
  ) ? Rule.do
    transitionProperty := none

  ( universal &. ClassName "next" |* a &:: after
      /\ universal &. ClassName "previous" |* a &:: after
      /\ universal &. ClassName "viewall" |* a &:: after
      /\ universal &. ClassName "zen-resources" |* a &:: after
      /\ universal &. ClassName "summary" |* p &: nthChild (nth 0 2) |* a &::
        after
  ) ? Rule.do
    opacity := 0

  -- Banner

  universal &@ role @= "banner" |* h1 ? Rule.do
    padding := em 1 ~ nil ~ nil
    textTransform := uppercase
    letterSpacing := em 0.1
    fontSize := rem 3.5
    fontWeight := 900
    color := hsl 216.0 0.53 0.2
    visibility := hidden
  universal &@ role @= "banner" |* h1 &:: before ? Rule.do
    content := "CSS Zen Garments"
    visibility := visible
    display := block
  universal &@ role @= "banner" |* h1 &:: after ? Rule.do
    content := "Made Locally"
    visibility := visible
    display := block
    letterSpacing := em 0.5
    fontSize := rem 1.4
    fontWeight := normal
    margin := em 0.5 ~ nil ~ em 3
    color := rgba 24 46 79 0.4

  universal &@ role @= "banner" |* h2 ? Rule.do
    visibility := hidden
    display := none
    textTransform := uppercase
    fontWeight := 400
    color := rgba 41 78 134 0.5
  universal &@ role @= "banner" |* h2 &:: after ? Rule.do
    content := "Impeccable Quality"
    visibility := visible
    display := block

  -- Preamble

  universal &. ClassName "summary" |* p ? Rule.do
    textTransform := uppercase
    letterSpacing := em 0.1
    fontSize := rem 1.8
    lineHeight := 1.6

  -- Preamble

  universal &. ClassName "preamble" ? Rule.do
    backgroundColor := transparent
    backgroundImage := url "i/sep.png"
    backgroundRepeat := noRepeat
    backgroundPosition := pct 50 ~ nil
  universal &. ClassName "preamble" |* h3 &:: after ? Rule.do
    visibility := visible
    content := "A Fashion-Forward Future"

  -- Explanation

  universal &. ClassName "explanation" |* h3 &:: after ? Rule.do
    content := "See Yourself in a Different Way"

  -- Participation

  universal &. ClassName "participation" |* h3 &:: after ? Rule.do
    content := "Get Into a Brand New Pair"

  -- Benefits

  universal &. ClassName "benefits" |* h3 &:: after ? Rule.do
    content := "Look Great\\2026  and Feel Great Too!"

  -- Requirements

  universal &. ClassName "requirements" |* h3 &:: after ? Rule.do
    content := "â€œOne Size Fitsâ€ All Be Damned!"
    textIndent := em (-0.5)

  -- Design Selection

  universal &. ClassName "design-selection" ? Rule.do
    margin := em 3 ~ nil ~ nil
  universal &. ClassName "design-selection" |* ul ? Rule.do
    listStyleType := none
    width := pct 94
    margin := nil ~ auto
  universal &. ClassName "design-selection" |* ul &:: after ? Rule.do
    content := "."
    display := block
    height := nil
    clear := both
    visibility := hidden
  universal &. ClassName "design-selection" |* li ? Rule.do
    float := left
    width := pct 50
    fontSize := rem 1.2
    margin := nil ~ nil ~ em 1
    color := hsl 0.0 0.0 0.6667
  universal &. ClassName "design-selection" |* li &: nthChild odd ? Rule.do
    width := pct 47
    paddingRight := pct 3
    clear := left

  universal &. ClassName "design-name" ? Rule.do
    display := block
    fontSize := rem 1.4
    fontWeight := bold
    margin := nil ~ nil ~ em 0.24
    textTransform := uppercase
    letterSpacing := em 0.1
    position := relative
  universal &. ClassName "design-name" &:: after ? Rule.do
    backgroundImage := none
    opacity := 0
  universal &. ClassName "design-name" &: hover ? Rule.do
    fontStyle := italic

  universal &. ClassName "designer-name" ? Rule.do
    color := rgb 121 137 163
  universal &. ClassName "designer-name" &:: after /\ footer |* a &:: after ?
    Rule.do
      height := px 1

  universal &. ClassName "select" &:: after ? Rule.do
    content := "Washes & Styles"
    fontSize := rem 1.8
    textAlign := center
    width := em 7
    margin := nil ~ auto ~ em 2
    backgroundColor := transparent
    backgroundImage := url "i/waves.png"
    backgroundRepeat := noRepeat
    backgroundPosition := pct 50 ~ nil
    padding := em 2 ~ nil ~ nil
    color := hsl 216.0 0.53 0.2
    visibility := visible

  universal &. ClassName "archives" /\ universal &. ClassName "resources" ?
    Rule.do
      position := absolute
      left := px (-9999)

  universal &. ClassName "design-archives" |* ul ? Rule.do
    listStyleType := none
    margin := em 1 ~ nil ~ nil
    width := pct 100
    textAlign := center
  universal &. ClassName "design-archives" |* li ? Rule.do
    display := inlineBlock
    position := relative
  universal &. ClassName "design-archives" |* a ? Rule.do
    display := block
    textIndent := pct 100
    whiteSpace := nowrap
    overflow := hidden

  universal &. ClassName "next" ? Rule.do
    marginRight := em 3
  universal &. ClassName "next" |* a ? Rule.do
    backgroundColor := transparent
    backgroundImage := url "s/czg.svg"
    backgroundRepeat := noRepeat
    backgroundPosition := px 10 ~ px (-56)
    width := px 43
    height := px 37
  ( universal &. ClassName "next" |* a &: hover
      /\ universal &. ClassName "next" |* a &: focus
  ) ? Rule.do
    backgroundPosition := px 10 ~ px (-138)

  universal &. ClassName "previous" ? Rule.do
    marginRight := em 3
  universal &. ClassName "previous" |* a ? Rule.do
    backgroundColor := transparent
    backgroundImage := url "s/czg.svg"
    backgroundRepeat := noRepeat
    backgroundPosition := px 10 ~ px (-220)
    width := px 43
    height := px 37
  ( universal &. ClassName "previous" |* a &: hover
      /\ universal &. ClassName "previous" |* a &: focus
  ) ? Rule.do
    backgroundPosition := px 10 ~ px (-302)

  universal &. ClassName "viewall" ? Rule.do
    marginLeft := em 1
  universal &. ClassName "viewall" |* a ? Rule.do
    backgroundColor := transparent
    backgroundImage := url "s/czg.svg"
    backgroundRepeat := noRepeat
    backgroundPosition := px (-56) ~ px (-55)
    width := px 39
    height := px 39
  ( universal &. ClassName "viewall" |* a &: hover
      /\ universal &. ClassName "viewall" |* a &: focus
  ) ? Rule.do
    backgroundPosition := px (-56) ~ px (-137)

  footer ? Rule.do
    fontSize := rem 1.3
  footer |* a ? Rule.do
    marginLeft := em 1
  footer |* a &: firstChild ? Rule.do
    marginLeft := nil

  universal &. ClassName "zen-resources" |* ul ? Rule.do
    listStyleType := none
    margin := em 2 ~ nil ~ em 4
    textAlign := center
  universal &. ClassName "zen-resources" |* ul &:: before ? Rule.do
    content := ""
    height := px 4
    width := px 80
    backgroundColor := rgb 213 217 220
    display := block
    textAlign := center
    margin := nil ~ auto ~ em 2
  universal &. ClassName "zen-resources" |* li ? Rule.do
    margin := nil ~ nil ~ em 1 ~ em 3
    display := inlineBlock
  universal &. ClassName "zen-resources" |* li &: firstChild ? Rule.do
    marginLeft := nil
  universal &. ClassName "zen-resources" |* a ? Rule.do
    display := inlineBlock
    textIndent := pct 100
    whiteSpace := nowrap
    overflow := hidden
    backgroundColor := transparent
    backgroundImage := url "s/czg.svg"
    backgroundRepeat := noRepeat
    backgroundPosition := nil ~ nil
    width := px 60
    height := px 60

  universal &. ClassName "view-css" |* a ? Rule.do
    backgroundPosition := px (-502) ~ px 12
  ( universal &. ClassName "view-css" |* a &: hover
      /\ universal &. ClassName "view-css" |* a &: focus
  ) ? Rule.do
    backgroundPosition := px (-502) ~ px (-78)

  universal &. ClassName "css-resources" |* a ? Rule.do
    backgroundPosition := px (-150) ~ px (-194)
  ( universal &. ClassName "css-resources" |* a &: hover
      /\ universal &. ClassName "css-resources" |* a &: focus
  ) ? Rule.do
    backgroundPosition := px (-150) ~ px (-286)

  universal &. ClassName "zen-faq" |* a ? Rule.do
    backgroundPosition := px (-265) ~ px (-192)
  ( universal &. ClassName "zen-faq" |* a &: hover
      /\ universal &. ClassName "zen-faq" |* a &: focus
  ) ? Rule.do
    backgroundPosition := px (-265) ~ px (-284)

  universal &. ClassName "zen-submit" |* a ? Rule.do
    backgroundPosition := px (-502) ~ px (-194)
  ( universal &. ClassName "zen-submit" |* a &: hover
      /\ universal &. ClassName "zen-submit" |* a &: focus
  ) ? Rule.do
    backgroundPosition := px (-502) ~ px (-286)

  universal &. ClassName "zen-translations" |* a ? Rule.do
    backgroundPosition := px (-380) ~ px (-193)
  ( universal &. ClassName "zen-translations" |* a &: hover
      /\ universal &. ClassName "zen-translations" |* a &: focus
  ) ? Rule.do
    backgroundPosition := px (-380) ~ px (-285)

  media screen { minWidth: px 700 } ? do

    body ? Rule.do
      backgroundAttachment := fixed
      backgroundPosition := pct 90 ~ pct 80

    universal &. ClassName "intro" &:: before ? Rule.do
      content := ""
      display := block
      width := px 105
      height := px 30
      position := fixed
      top := px 154
      left := pct 3
      backgroundColor := transparent
      backgroundImage := url "s/czg.svg"
      backgroundRepeat := noRepeat
      backgroundPosition := px (-184) ~ px (-99)

    ( universal &@ role @= "article" /\ universal &@ role @= "banner" /\ footer
    ) ? Rule.do
      padding := nil ~ pct 30 ~ em 10 ~ pct 10
      textAlign := right
      position := relative
    ( universal &@ role @= "article" |* h3
        /\ universal &@ role @= "banner" |* h2
    ) ? Rule.do
      position := absolute
      top := em (-0.75)
      left := pct 75
      width := em 10
      textAlign := left
      fontSize := rem 1.5
      letterSpacing := nil
    universal &@ role @= "article" |* h3 &:: after ? Rule.do
      position := absolute
      top := nil

    universal &@ role @= "banner" ? Rule.do
      paddingTop := px 200
      paddingBottom := em 3

    universal &@ role @= "banner" |* h1 ? Rule.do
      fontSize := rem 4.8
      letterSpacing := nil
      lineHeight := 0.8
    universal &@ role @= "banner" |* h1 &:: after ? Rule.do
      position := absolute
      top := em 15.7
      right := pct 30

    universal &@ role @= "banner" |* h2 ? Rule.do
      display := block
      top := em 8.1
      width := em 3
      left := pct 75.5
      fontSize := rem 1.9
      lineHeight := 1
    universal &@ role @= "banner" |* h2 &:: before ? Rule.do
      visibility := visible
      content := "Est. 2003"
      display := block
      position := fixed
      top := em (-0.1)
      bottom := nil
      left := em (-0.5)
      right := nil
      margin := auto
      fontWeight := 900
      fontSize := rem 50
      color := rgba 0 0 0 0.15
      width := pct 100
      height := em 2
      lineHeight := 1
      transform := rotate $ deg (-90)
      maskImage := url "i/denim-mask2.png"

    universal &. ClassName "summary" |* p &: nthChild (nth 0 2) ? Rule.do
      backgroundColor := rgb 255 255 0
      position := relative
      visibility := hidden
      top := em 2
      fontSize := rem 1.4

    universal &. ClassName "summary" |* p &: nthChild (nth 0 2) &:: before ?
      Rule.do
        content := ""
        display := block
        visibility := visible
        position := absolute
        right := em 21
        top := em (-0.25)
        backgroundColor := transparent
        backgroundImage := url "s/czg.svg"
        backgroundRepeat := noRepeat
        backgroundPosition := px (-159) ~ px (-1)
        width := em 6
        height := px 30

    universal &. ClassName "summary" |* p &: nthChild (nth 0 2) &:: after ?
      Rule.do
        content := ""
        display := block
        visibility := visible
        position := absolute
        right := em 14
        top := em (-0.25)
        backgroundColor := transparent
        backgroundImage := url "s/czg.svg"
        backgroundRepeat := noRepeat
        backgroundPosition := px (-277) ~ nil
        width := em 6
        height := px 30

    universal &. ClassName "summary" |* p &: nthChild (nth 0 2) |* a ? Rule.do
      visibility := visible
      backgroundColor := transparent
      backgroundImage := url "s/czg.svg"
      backgroundRepeat := noRepeat
      backgroundPosition := px (-376) ~ px (-93)
      width := em 6
      height := px 25
      textIndent := pct 100
      whiteSpace := nowrap
      overflow := hidden
      display := block
      position := absolute
      top := nil
      right := em 7
    ( universal &. ClassName "summary" |* p &: nthChild (nth 0 2) |* a &: hover
        /\ universal &. ClassName "summary" |* p &: nthChild (nth 0 2) |* a &:
          focus
    ) ? Rule.do
      backgroundPosition := px (-396) ~ px (-3)
      textIndent := nil
      backgroundImage := none

    ( universal &. ClassName "summary" |* p &: nthChild (nth 0 2) |* a &@ href
        *= "css"
    ) ? Rule.do
      backgroundPosition := px (-472) ~ px (-94)
      right := nil
    ( universal &. ClassName "summary" |* p &: nthChild (nth 0 2) |* a &@ href
        *= "css"
        &:
          hover
        /\ universal &. ClassName "summary" |* p &: nthChild (nth 0 2) |* a
          &@ href
          *= "css"
          &: focus
    ) ? Rule.do
      backgroundPosition := px (-512) ~ px (-4)

    universal &. ClassName "preamble" ? Rule.do
      paddingTop := em 10
    universal &. ClassName "preamble" |* h3 ? Rule.do
      top := em 5.9

    universal &. ClassName "design-selection" |* li ? Rule.do
      width := pct 23
      marginBottom := em 3
      paddingRight := pct 2
    universal &. ClassName "design-selection" |* li &: nthChild odd ? Rule.do
      width := pct 23
      clear := none
      paddingRight := pct 2

    universal &. ClassName "next" ? Rule.do
      marginRight := em 2
      marginLeft := em (-0.4)
    universal &. ClassName "viewall" ? Rule.do
      marginLeft := nil

    footer ? Rule.do
      paddingBottom := em 3
      marginTop := em (-5)

  media screen { minWidth: px 1130 } ? do

    universal &@ role @= "article" /\ universal &@ role @= "banner" ? Rule.do
      paddingLeft := pct 25

    universal &. ClassName "design-selection" ? Rule.do
      position := absolute
      left := pct 5
      top := em 30
    universal &. ClassName "design-selection" |* ul ? Rule.do
      margin := nil
      width := pct 100
    universal &. ClassName "design-selection" |* li ? Rule.do
      float := none
      marginBottom := em 2
    ( universal
        &. ClassName "design-selection"
        &. ClassName "design-selection" -- adding specificity in lieu of !important
        &. ClassName "design-selection"
        |* li
    ) ? Rule.do
      width := pct 100
      padding := nil
    universal &. ClassName "select" &:: after ? Rule.do
      textAlign := left
      backgroundPosition := nil ~ bottom
      padding := nil ~ nil ~ em 2
      margin := nil ~ nil ~ em 2

    universal &. ClassName "design-archives" ? Rule.do
      position := absolute
      left := pct 5
      top := em 95

  media screen { minWidth: px 1130, minHeight: px 1037 } ? do
    universal &. ClassName "design-selection" /\ universal
      &. ClassName "design-archives"
      ? Rule.do
          position := fixed

  -- Animations

  let fadey = KeyframesName "FADEY"

  keyframes fadey ? do
    pct 0 ? Rule.do
      opacity := 0
    pct 100 ? Rule.do
      opacity := 1

  universal &. ClassName "intro" ? Rule.do
    animationName := fadey
    animationDuration := sec 1
    animationTimingFunction := easeInOut
    animationIterationCount := 1

  universal &@ role @= "article" ? Rule.do
    opacity := 0
    animationName := fadey
    animationDuration := sec 1
    animationTimingFunction := easeInOut
    animationDelay := sec 0.5
    animationFillMode := forwards
    animationIterationCount := 1

  universal &. ClassName "design-selection" /\ universal
    &. ClassName "design-archives"
    ? Rule.do
        opacity := 0
        animationName := fadey
        animationDuration := sec 1
        animationTimingFunction := easeInOut
        animationDelay := sec 1
        animationFillMode := forwards
        animationIterationCount := 1
