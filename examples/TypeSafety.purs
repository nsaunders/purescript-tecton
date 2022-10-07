module Example.TypeSafety where

import Prelude

import Color (black, white)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Tecton (backgroundImage, color, currentColor, deg, fontFace, fontFamily, keyframes, keyframesName, linearGradient, marginLeft, opacity, pct, placeholder, pretty, render, sansSerif, src, transitionProperty, universal, url, (&.), (&:), (:=), (?))
-- import Tecton (after, alignContent, content)
import Tecton.Rule as Rule

main :: Effect Unit
main = log $ render pretty do

  -- Animations

  let fade = keyframesName "fade"

  {-
  -- Error: Not an animatable property
  keyframes fade ? do
    pct 0 ? content := ""
    pct 100 ? content := "hello"
  -}

  -- Works
  keyframes fade ? do
    pct 0 ? opacity := 0
    pct 100 ? opacity := 1

  -- Fonts

  {-
  -- Error: Missing `src` descriptor
  fontFace ? fontFamily := "Foo"
  -}

  {-
  -- Error: Missing `font-family` descriptor
  fontFace ? src := url "foo.woff"
  -}

  -- Works
  fontFace ? Rule.do
    fontFamily := "Foo"
    src := url "foo.woff"

  {-
  -- Error: No generic font fallback
  universal ? fontFamily := "Roboto" /\ "Arial"
  -}

  -- Works
  universal ? fontFamily := sansSerif

  -- Works
  universal ? fontFamily := "Roboto" /\ "Arial" /\ sansSerif

  -- Images

  {-
  -- Error: Insufficient color stops
  universal &. "fade-to-black" ?
    backgroundImage := linearGradient (deg 180) black
  -}

  {-
  -- Error: Consecutive transition hints
  universal &. "fade-to-black" ?
    backgroundImage :=
        linearGradient (deg 180) $ white /\ pct 60 /\ pct 70 /\ black
  -}

  -- Works
  universal &. "fade-to-black" ?
    backgroundImage := linearGradient (deg 180) $ white /\ pct 70 /\ black

  -- Selectors

  {-
  -- Error: Multiple pseudo-elements
  universal &: placeholder &: after ? color := currentColor
  -}

  -- Works
  universal &: placeholder ? color := currentColor

  -- Transitions
  
  {-
  -- Error: Not an animatable property
  universal ? transitionProperty := alignContent
  -}

  -- Works
  universal ? transitionProperty := marginLeft
