-- https://www.w3.org/TR/selectors-3/
-- https://www.w3.org/TR/selectors-4/
-- https://www.w3.org/TR/css-pseudo-4/

module Test.SelectorsSpec where

import Prelude hiding (not)

import Color (rgb)
import Data.Tuple.Nested ((/\))
import Tecton
  ( AttrName(..)
  , ClassName(..)
  , ElementId(..)
  , PseudoClass(..)
  , PseudoElement(..)
  , a
  , active
  , after
  , backgroundColor
  , before
  , checked
  , disabled
  , empty
  , enabled
  , even
  , firstChild
  , firstLetter
  , firstLine
  , firstOfType
  , focus
  , focusWithin
  , hover
  , href
  , hreflang
  , indeterminate
  , lang
  , lastChild
  , lastOfType
  , link
  , nil
  , not
  , nthChild
  , nthLastChild
  , nthOfType
  , odd
  , onlyChild
  , onlyOfType
  , placeholder
  , root
  , selection
  , span
  , target
  , title
  , universal
  , visited
  , width
  , (#+)
  , (#-)
  , ($=)
  , (&#)
  , (&.)
  , (&:)
  , (&::)
  , (&@)
  , (*=)
  , (:=)
  , (?)
  , (@=)
  , (^=)
  , (|*)
  , (|+)
  , (|=)
  , (|>)
  , (|~)
  , (~=)
  )
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFromSheet)

spec :: Spec Unit
spec = do

  let isRenderedFrom = isRenderedFromSheet

  describe "Selectors Module" do

    describe "Universal selector" do

      "*{width:0}" `isRenderedFrom` do universal ? width := nil

    describe "Element selectors" do

      "a{width:0}" `isRenderedFrom` do a ? width := nil

      "span{width:0}" `isRenderedFrom` do span ? width := nil

    describe "Attribute selectors" do

      "*[href]{width:0}"
        `isRenderedFrom` do
          universal &@ href ? width := nil

      "*[href=\"http://www.w3.org/\"]{width:0}"
        `isRenderedFrom` do
          universal &@ href @= "http://www.w3.org/" ? width := nil

      "*[data-states~=\"selected\"]{width:0}"
        `isRenderedFrom` do
          universal &@ AttrName "data-states" ~= "selected" ? width := nil

      "*[hreflang|=\"en\"]{width:0}"
        `isRenderedFrom` do
          universal &@ hreflang |= "en" ? width := nil

      "*[data-timezone^=\"UTC-\"]{width:0}"
        `isRenderedFrom` do
          universal &@ AttrName "data-timezone" ^= "UTC-" ? width := nil

      "*[data-timezone$=\":30\"]{width:0}"
        `isRenderedFrom` do
          universal &@ AttrName "data-timezone" $= ":30" ? width := nil

      "*[title*=\"hello\"]{width:0}"
        `isRenderedFrom` do
          universal &@ title *= "hello" ? width := nil

    describe "Class selectors" do

      "*.pastoral{width:0}"
        `isRenderedFrom` do
          universal &. ClassName "pastoral" ? width := nil

      "*.pastoral.marine{width:0}"
        `isRenderedFrom` do
          universal &. ClassName "pastoral" &. ClassName "marine" ? width := nil

    describe "ID selectors" do

      "*#chapter1{width:0}"
        `isRenderedFrom` do
          universal &# ElementId "chapter1" ? width := nil

      "*#z98y{width:0}" `isRenderedFrom` do
        universal &# ElementId "z98y" ? width := nil

    describe "Pseudo-classes" do

      "*:link{width:0}" `isRenderedFrom` do universal &: link ? width := nil

      "*:visited{width:0}"
        `isRenderedFrom` do
          universal &: visited ? width := nil

      "*:hover{width:0}" `isRenderedFrom` do universal &: hover ? width := nil

      "*:focus{width:0}" `isRenderedFrom` do universal &: focus ? width := nil

      "*:active{width:0}" `isRenderedFrom` do universal &: active ? width := nil

      "*:target{width:0}" `isRenderedFrom` do universal &: target ? width := nil

      "*:lang(en-US){width:0}"
        `isRenderedFrom` do
          universal &: lang "en-US" ? width := nil

      "*:enabled{width:0}"
        `isRenderedFrom` do
          universal &: enabled ? width := nil

      "*:disabled{width:0}"
        `isRenderedFrom` do
          universal &: disabled ? width := nil

      "*:checked{width:0}"
        `isRenderedFrom` do
          universal &: checked ? width := nil

      "*:indeterminate{width:0}"
        `isRenderedFrom` do
          universal &: indeterminate ? width := nil

      "*:root{width:0}" `isRenderedFrom` do universal &: root ? width := nil

      "*:nth-child(2n){width:0}"
        `isRenderedFrom` do
          universal &: nthChild even ? width := nil

      "*:nth-child(2n+1){width:0}"
        `isRenderedFrom` do
          universal &: nthChild odd ? width := nil

      "*:nth-child(2n){width:0}"
        `isRenderedFrom` do
          universal &: nthChild (2 #+ 0) ? width := nil

      "*:nth-child(2n+1){width:0}"
        `isRenderedFrom` do
          universal &: nthChild (2 #+ 1) ? width := nil

      "*:nth-child(10n-1){width:0}"
        `isRenderedFrom` do
          universal &: nthChild (10 #- 1) ? width := nil

      "*:nth-last-child(-n+2){width:0}"
        `isRenderedFrom` do
          universal &: nthLastChild ((-1) #+ 2) ? width := nil

      "*:nth-last-child(2n+1){width:0}"
        `isRenderedFrom` do
          universal &: nthLastChild odd ? width := nil

      "*:nth-of-type(2n+1){width:0}"
        `isRenderedFrom` do
          universal &: nthOfType (2 #+ 1) ? width := nil

      "*:nth-of-type(2n){width:0}"
        `isRenderedFrom` do
          universal &: nthOfType (2 #+ 0) ? width := nil

      "*:first-child{width:0}"
        `isRenderedFrom` do
          universal &: firstChild ? width := nil

      "*:last-child{width:0}"
        `isRenderedFrom` do
          universal &: lastChild ? width := nil

      "*:first-of-type{width:0}"
        `isRenderedFrom` do
          universal &: firstOfType ? width := nil

      "*:last-of-type{width:0}"
        `isRenderedFrom` do
          universal &: lastOfType ? width := nil

      "*:only-child{width:0}"
        `isRenderedFrom` do
          universal &: onlyChild ? width := nil

      "*:only-of-type{width:0}"
        `isRenderedFrom` do
          universal &: onlyOfType ? width := nil

      "*:empty{width:0}" `isRenderedFrom` do universal &: empty ? width := nil

      "*:not(*){width:0}"
        `isRenderedFrom` do
          universal &: not universal ? width := nil

      "*:not(*.foo~*:checked){width:0}"
        `isRenderedFrom` do
          universal
            &: not (universal &. ClassName "foo" |~ universal &: checked)
            ? do
                width := nil

      "*:not(*,*){width:0}"
        `isRenderedFrom` do
          universal &: not (universal /\ universal) ? width := nil

      "*:focus-within{width:0}"
        `isRenderedFrom` do
          universal &: focusWithin ? width := nil

      "*:-moz-user-disabled{background-color:#ff0000}"
        `isRenderedFrom` do
          universal &: PseudoClass "-moz-user-disabled"
            ? backgroundColor
            := rgb 255 0 0

    describe "Pseudo-elements" do

      "*::first-line{width:0}"
        `isRenderedFrom` do
          universal &:: firstLine ? width := nil

      "*::first-letter{width:0}"
        `isRenderedFrom` do
          universal &:: firstLetter ? width := nil

      "*::before{width:0}"
        `isRenderedFrom` do
          universal &:: before ? width := nil

      "*::after{width:0}"
        `isRenderedFrom` do
          universal &:: after ? width := nil

      "*::placeholder{width:0}"
        `isRenderedFrom` do
          universal &:: placeholder ? width := nil

      "*::selection{width:0}"
        `isRenderedFrom` do
          universal &:: selection ? width := nil

      "*::-webkit-meter-bar{background-color:#eeeeee}"
        `isRenderedFrom` do
          universal &:: PseudoElement "-webkit-meter-bar" ? backgroundColor :=
            rgb 238 238 238

    describe "Combinators" do

      "* *{width:0}" `isRenderedFrom` do universal |* universal ? width := nil

      "*>*{width:0}" `isRenderedFrom` do universal |> universal ? width := nil

      "*+*{width:0}" `isRenderedFrom` do universal |+ universal ? width := nil

      "*~*{width:0}" `isRenderedFrom` do universal |~ universal ? width := nil

    describe "Groups of selectors" do

      "*:checked,*.checked{width:0}"
        `isRenderedFrom` do
          universal &: checked /\ universal &. ClassName "checked" ? width :=
            nil

      "*.foo,*::after{width:0}"
        `isRenderedFrom` do
          universal &. ClassName "foo" /\ universal &:: after ? width := nil
