-- https://www.w3.org/TR/selectors-3/
-- https://www.w3.org/TR/selectors-4/

module Test.SelectorsSpec where

import Prelude hiding (not)

import Data.Tuple.Nested ((/\))
import PSCSS (active, after, att, before, checked, disabled, empty, enabled, even, firstChild, firstLine, firstLetter, firstOfType, focus, hover, href, hreflang, indeterminate, lang, lastChild, lastOfType, link, not, nth, nthChild, nthLastChild, nthOfType, odd, onlyChild, onlyOfType, root, target, title, universal, visited, (&@), (&.), (&#), (&:), ($=), (*=), (?), (@=), (^=), (|*), (|+), (|=), (|>), (|~), (~=))
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFrom)

spec :: Spec Unit
spec =
  describe "Selectors Module" do

    describe "Universal selector" do

      "*{}" `isRenderedFrom` do universal ? {}

    describe "Attribute selectors" do

      "*[href]{}" `isRenderedFrom` do universal &@ href ? {}

      "*[href=\"http://www.w3.org/\"]{}"
        `isRenderedFrom` do
        universal &@ href @= "http://www.w3.org/" ? {}

      "*[data-states~=\"selected\"]{}"
        `isRenderedFrom` do
        universal &@ att "data-states" ~= "selected" ? {}

      "*[hreflang|=\"en\"]{}"
        `isRenderedFrom` do
        universal &@ hreflang |= "en" ? {}

      "*[data-timezone^=\"UTC-\"]{}"
        `isRenderedFrom` do
        universal &@ att "data-timezone" ^= "UTC-" ? {}

      "*[data-timezone$=\":30\"]{}"
        `isRenderedFrom` do
        universal &@ att "data-timezone" $= ":30" ? {}

      "*[title*=\"hello\"]{}"
        `isRenderedFrom` do
        universal &@ title *= "hello" ? {}

    describe "Class selectors" do

      "*.pastoral{}" `isRenderedFrom` do universal &. "pastoral" ? {}

      "*.pastoral.marine{}"
        `isRenderedFrom` do
        universal &. "pastoral" &. "marine" ? {}

    describe "ID selectors" do

      "*#chapter1{}" `isRenderedFrom` do universal &# "chapter1" ? {}

      "*#z98y{}" `isRenderedFrom` do universal &# "z98y" ? {}

    describe "Pseudo-classes" do

      "*:link{}" `isRenderedFrom` do universal &: link ? {}

      "*:visited{}" `isRenderedFrom` do universal &: visited ? {}

      "*:hover{}" `isRenderedFrom` do universal &: hover ? {}

      "*:focus{}" `isRenderedFrom` do universal &: focus ? {}

      "*:active{}" `isRenderedFrom` do universal &: active ? {}

      "*:target{}" `isRenderedFrom` do universal &: target ? {}

      "*:lang(en-US){}" `isRenderedFrom` do universal &: lang "en-US" ? {}

      "*:enabled{}" `isRenderedFrom` do universal &: enabled ? {}

      "*:disabled{}" `isRenderedFrom` do universal &: disabled ? {}

      "*:checked{}" `isRenderedFrom` do universal &: checked ? {}

      "*:indeterminate{}" `isRenderedFrom` do universal &: indeterminate ? {}

      "*:root{}" `isRenderedFrom` do universal &: root ? {}

      "*:nth-child(even){}" `isRenderedFrom` do universal &: nthChild even ? {}

      "*:nth-child(odd){}" `isRenderedFrom` do universal &: nthChild odd ? {}

      "*:nth-child(2n){}"
        `isRenderedFrom` do
        universal &: nthChild (nth 2 0) ? {}

      "*:nth-child(2n+1){}"
        `isRenderedFrom` do
        universal &: nthChild (nth 2 1) ? {}

      "*:nth-child(10n-1){}"
        `isRenderedFrom` do
        universal &: nthChild (nth 10 (-1)) ? {}

      "*:nth-last-child(-n+2){}"
        `isRenderedFrom` do
        universal &: nthLastChild (nth (-1) 2) ? {}

      "*:nth-last-child(odd){}"
        `isRenderedFrom` do
        universal &: nthLastChild odd ? {}

      "*:nth-of-type(2n+1){}"
        `isRenderedFrom` do
        universal &: nthOfType (nth 2 1) ? {}

      "*:nth-of-type(2n){}"
        `isRenderedFrom` do
        universal &: nthOfType (nth 2 0) ? {}

      "*:first-child{}" `isRenderedFrom` do universal &: firstChild ? {}

      "*:last-child{}" `isRenderedFrom` do universal &: lastChild ? {}

      "*:first-of-type{}" `isRenderedFrom` do universal &: firstOfType ? {}

      "*:last-of-type{}" `isRenderedFrom` do universal &: lastOfType ? {}

      "*:only-child{}" `isRenderedFrom` do universal &: onlyChild ? {}

      "*:only-of-type{}" `isRenderedFrom` do universal &: onlyOfType ? {}

      "*:empty{}" `isRenderedFrom` do universal &: empty ? {}

      "*:not(*){}" `isRenderedFrom` do universal &: not universal ? {}

      "*:not(*.foo~*:checked){}"
        `isRenderedFrom` do
        universal &: not (universal &. "foo" |~ universal &: checked) ? {}

      "*:not(*,*){}"
        `isRenderedFrom` do
        universal &: not (universal /\ universal) ? {}

    describe "Pseudo-elements" do

      "*::first-line{}" `isRenderedFrom` do universal &: firstLine ? {}

      "*::first-letter{}" `isRenderedFrom` do universal &: firstLetter ? {}

      "*::before{}" `isRenderedFrom` do universal &: before ? {}

      "*::after{}" `isRenderedFrom` do universal &: after ? {}

    describe "Combinators" do

      "* *{}" `isRenderedFrom` do universal |* universal ? {}

      "*>*{}" `isRenderedFrom` do universal |> universal ? {}

      "*+*{}" `isRenderedFrom` do universal |+ universal ? {}

      "*~*{}" `isRenderedFrom` do universal |~ universal ? {}

    describe "Groups of selectors" do

      "*:checked,*.checked{}"
        `isRenderedFrom` do
        universal &: checked /\ universal &. "checked" ? {}

      "*.foo,*::after{}"
        `isRenderedFrom` do
        universal &. "foo" /\ universal &: after ? {}
