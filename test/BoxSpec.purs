-- https://www.w3.org/TR/css-box-3/

module Test.BoxSpec where

import Prelude

import Tecton
  ( auto
  , em
  , inherit
  , initial
  , margin
  , marginBottom
  , marginLeft
  , marginRight
  , marginTop
  , padding
  , paddingBottom
  , paddingLeft
  , paddingRight
  , paddingTop
  , pct
  , px
  , unset
  , (:=)
  , (~)
  )
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFromInline)

spec :: Spec Unit
spec = do

  let isRenderedFrom = isRenderedFromInline

  describe "Box Module" do

    describe "margin-top property" do

      "margin-top:inherit" `isRenderedFrom` (marginTop := inherit)

      "margin-top:initial" `isRenderedFrom` (marginTop := initial)

      "margin-top:unset" `isRenderedFrom` (marginTop := unset)

      "margin-top:1px" `isRenderedFrom` (marginTop := px 1)

      "margin-top:10%" `isRenderedFrom` (marginTop := pct 10)

      "margin-top:auto" `isRenderedFrom` (marginTop := auto)

    describe "margin-right property" do

      "margin-right:inherit" `isRenderedFrom` (marginRight := inherit)

      "margin-right:initial" `isRenderedFrom` (marginRight := initial)

      "margin-right:unset" `isRenderedFrom` (marginRight := unset)

      "margin-right:1px" `isRenderedFrom` (marginRight := px 1)

      "margin-right:10%" `isRenderedFrom` (marginRight := pct 10)

      "margin-right:auto" `isRenderedFrom` (marginRight := auto)

    describe "margin-bottom property" do

      "margin-bottom:inherit" `isRenderedFrom` (marginBottom := inherit)

      "margin-bottom:initial" `isRenderedFrom` (marginBottom := initial)

      "margin-bottom:unset" `isRenderedFrom` (marginBottom := unset)

      "margin-bottom:1px" `isRenderedFrom` (marginBottom := px 1)

      "margin-bottom:10%" `isRenderedFrom` (marginBottom := pct 10)

      "margin-bottom:auto" `isRenderedFrom` (marginBottom := auto)

    describe "margin-left property" do

      "margin-left:inherit" `isRenderedFrom` (marginLeft := inherit)

      "margin-left:initial" `isRenderedFrom` (marginLeft := initial)

      "margin-left:unset" `isRenderedFrom` (marginLeft := unset)

      "margin-left:1px" `isRenderedFrom` (marginLeft := px 1)

      "margin-left:10%" `isRenderedFrom` (marginLeft := pct 10)

      "margin-left:auto" `isRenderedFrom` (marginLeft := auto)

    describe "margin property" do

      "margin:inherit" `isRenderedFrom` (margin := inherit)

      "margin:initial" `isRenderedFrom` (margin := initial)

      "margin:unset" `isRenderedFrom` (margin := unset)

      "margin:1px" `isRenderedFrom` (margin := px 1)

      "margin:10%" `isRenderedFrom` (margin := pct 10)

      "margin:1px 10%" `isRenderedFrom` (margin := px 1 ~ pct 10)

      "margin:10% 1px 25%" `isRenderedFrom` (margin := pct 10 ~ px 1 ~ pct 25)

      "margin:1px 1em 25% auto"
        `isRenderedFrom`
          (margin := px 1 ~ em 1 ~ pct 25 ~ auto)

    describe "padding-top property" do

      "padding-top:inherit" `isRenderedFrom` (paddingTop := inherit)

      "padding-top:initial" `isRenderedFrom` (paddingTop := initial)

      "padding-top:unset" `isRenderedFrom` (paddingTop := unset)

      "padding-top:1px" `isRenderedFrom` (paddingTop := px 1)

      "padding-top:10%" `isRenderedFrom` (paddingTop := pct 10)

    describe "padding-right property" do

      "padding-right:inherit" `isRenderedFrom` (paddingRight := inherit)

      "padding-right:initial" `isRenderedFrom` (paddingRight := initial)

      "padding-right:unset" `isRenderedFrom` (paddingRight := unset)

      "padding-right:1px" `isRenderedFrom` (paddingRight := px 1)

      "padding-right:10%" `isRenderedFrom` (paddingRight := pct 10)

    describe "padding-bottom property" do

      "padding-bottom:inherit" `isRenderedFrom` (paddingBottom := inherit)

      "padding-bottom:initial" `isRenderedFrom` (paddingBottom := initial)

      "padding-bottom:unset" `isRenderedFrom` (paddingBottom := unset)

      "padding-bottom:1px" `isRenderedFrom` (paddingBottom := px 1)

      "padding-bottom:10%" `isRenderedFrom` (paddingBottom := pct 10)

    describe "padding-left property" do

      "padding-left:inherit" `isRenderedFrom` (paddingLeft := inherit)

      "padding-left:initial" `isRenderedFrom` (paddingLeft := initial)

      "padding-left:unset" `isRenderedFrom` (paddingLeft := unset)

      "padding-left:1px" `isRenderedFrom` (paddingLeft := px 1)

      "padding-left:10%" `isRenderedFrom` (paddingLeft := pct 10)

    describe "padding property" do

      "padding:inherit" `isRenderedFrom` (padding := inherit)

      "padding:initial" `isRenderedFrom` (padding := initial)

      "padding:unset" `isRenderedFrom` (padding := unset)

      "padding:1px" `isRenderedFrom` (padding := px 1)

      "padding:10%" `isRenderedFrom` (padding := pct 10)

      "padding:10% 20%" `isRenderedFrom` (padding := pct 10 ~ pct 20)

      "padding:1px 10% 2em" `isRenderedFrom` (padding := px 1 ~ pct 10 ~ em 2)

      "padding:2em 1% 1px 10%"
        `isRenderedFrom`
          (padding := em 2 ~ pct 1 ~ px 1 ~ pct 10)
