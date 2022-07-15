-- https://www.w3.org/TR/selectors-3/
-- https://www.w3.org/TR/selectors-4/

module Test.SelectorsSpec where

import Prelude

import Data.NonEmpty ((:|))
import PSCSS (att, byActive, byAfter, byAtt, byBefore, byChecked, byClass, byDisabled, byEmpty, byEnabled, byFirstChild, byFirstLetter, byFirstLine, byFirstOfType, byFocus, byHover, byId, byIndeterminate, byLang, byLastChild, byLastOfType, byLink, byNot, byNthChild, byNthLastChild, byNthOfType, byOnlyChild, byOnlyOfType, byRoot, byTarget, byVisited, even, href, hreflang, nth, odd, title, universal, ($=), (*=), (?), (@=), (^=), (|*), (|+), (|=), (|>), (|~), (~=))
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFrom)

spec :: Spec Unit
spec =
  describe "Selectors Module" do

    describe "Universal selector" do

      "*{}" `isRenderedFrom` do universal ? {}

    describe "Attribute selectors" do

      "*[href]{}" `isRenderedFrom` do universal # byAtt href ? {}

      "*[href=\"http://www.w3.org/\"]{}"
        `isRenderedFrom` do
        universal # href @= "http://www.w3.org/" ? {}

      "*[data-states~=\"selected\"]{}"
        `isRenderedFrom` do
        universal # att "data-states" ~= "selected" ? {}

      "*[hreflang|=\"en\"]{}"
        `isRenderedFrom` do
        universal # hreflang |= "en" ? {}

      "*[data-timezone^=\"UTC-\"]{}"
        `isRenderedFrom` do
        universal # att "data-timezone" ^= "UTC-" ? {}

      "*[data-timezone$=\":30\"]{}"
        `isRenderedFrom` do
        universal # att "data-timezone" $= ":30" ? {}

      "*[title*=\"hello\"]{}"
        `isRenderedFrom` do
        universal # title *= "hello" ? {}

    describe "Class selectors" do

      "*.pastoral{}" `isRenderedFrom` do universal # byClass "pastoral" ? {}

      "*.pastoral.marine{}"
        `isRenderedFrom` do
        universal # byClass "pastoral" # byClass "marine" ? {}

    describe "ID selectors" do

      "*#chapter1{}" `isRenderedFrom` do universal # byId "chapter1" ? {}

      "*#z98y{}" `isRenderedFrom` do universal # byId "z98y" ? {}

    describe "Pseudo-classes" do

      "*:link{}" `isRenderedFrom` do universal # byLink ? {}

      "*:visited{}" `isRenderedFrom` do universal # byVisited ? {}

      "*:hover{}" `isRenderedFrom` do universal # byHover ? {}

      "*:focus{}" `isRenderedFrom` do universal # byFocus ? {}

      "*:active{}" `isRenderedFrom` do universal # byActive ? {}

      "*:target{}" `isRenderedFrom` do universal # byTarget ? {}

      "*:lang(en-US){}" `isRenderedFrom` do universal # byLang "en-US" ? {}

      "*:enabled{}" `isRenderedFrom` do universal # byEnabled ? {}

      "*:disabled{}" `isRenderedFrom` do universal # byDisabled ? {}

      "*:checked{}" `isRenderedFrom` do universal # byChecked ? {}

      "*:indeterminate{}" `isRenderedFrom` do universal # byIndeterminate ? {}

      "*:root{}" `isRenderedFrom` do universal # byRoot ? {}

      "*:nth-child(even){}" `isRenderedFrom` do universal # byNthChild even ? {}

      "*:nth-child(odd){}" `isRenderedFrom` do universal # byNthChild odd ? {}

      "*:nth-child(2n){}"
        `isRenderedFrom` do
        universal # byNthChild (nth 2 0) ? {}

      "*:nth-child(2n+1){}"
        `isRenderedFrom` do
        universal # byNthChild (nth 2 1) ? {}

      "*:nth-child(10n-1){}"
        `isRenderedFrom` do
        universal # byNthChild (nth 10 (-1)) ? {}

      "*:nth-last-child(-n+2){}"
        `isRenderedFrom` do
        universal # byNthLastChild (nth (-1) 2) ? {}

      "*:nth-last-child(odd){}"
        `isRenderedFrom` do
        universal # byNthLastChild odd ? {}

      "*:nth-of-type(2n+1){}"
        `isRenderedFrom` do
        universal # byNthOfType (nth 2 1) ? {}

      "*:nth-of-type(2n){}"
        `isRenderedFrom` do
        universal # byNthOfType (nth 2 0) ? {}

      "*:first-child{}" `isRenderedFrom` do universal # byFirstChild ? {}

      "*:last-child{}" `isRenderedFrom` do universal # byLastChild ? {}

      "*:first-of-type{}" `isRenderedFrom` do universal # byFirstOfType ? {}

      "*:last-of-type{}" `isRenderedFrom` do universal # byLastOfType ? {}

      "*:only-child{}" `isRenderedFrom` do universal # byOnlyChild ? {}

      "*:only-of-type{}" `isRenderedFrom` do universal # byOnlyOfType ? {}

      "*:empty{}" `isRenderedFrom` do universal # byEmpty ? {}

      "*:not(*){}" `isRenderedFrom` do universal # byNot universal ? {}

      "*:not(*.foo~*:checked){}"
        `isRenderedFrom` do
        universal # byNot (universal # byClass "foo" |~ universal # byChecked) ?
          {}

      "*:not(*,*){}"
        `isRenderedFrom` do
        universal # byNot (universal :| [universal]) ? {}

    describe "Pseudo-elements" do

      "*::first-line{}" `isRenderedFrom` do universal # byFirstLine ? {}

      "*::first-letter{}" `isRenderedFrom` do universal # byFirstLetter ? {}

      "*::before{}" `isRenderedFrom` do universal # byBefore ? {}

      "*::after{}" `isRenderedFrom` do universal # byAfter ? {}

    describe "Combinators" do

      "* *{}" `isRenderedFrom` do universal |* universal ? {}

      "* *{}" `isRenderedFrom` do universal |* universal ? {}

      "*>*{}" `isRenderedFrom` do universal |> universal ? {}

      "*+*{}" `isRenderedFrom` do universal |+ universal ? {}

      "*~*{}" `isRenderedFrom` do universal |~ universal ? {}

    describe "Groups of selectors" do

      "*:checked,*.checked{}"
        `isRenderedFrom` do
        (universal # byChecked) :| [universal # byClass "checked"] ? {}
