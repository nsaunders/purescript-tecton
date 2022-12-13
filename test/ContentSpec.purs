-- https://www.w3.org/TR/css-content-3/

module Test.ContentSpec where

import Prelude

import Color (rgb)
import Data.Tuple.Nested ((/\))
import Tecton
  ( content
  , contents
  , inherit
  , initial
  , linearGradient
  , nil
  , none
  , normal
  , unset
  , url
  , (:=)
  )
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFromInline)

spec :: Spec Unit
spec = do

  let isRenderedFrom = isRenderedFromInline

  describe "Generated Content Module" do

    describe "content property" do

      "content:inherit" `isRenderedFrom` (content := inherit)

      "content:initial" `isRenderedFrom` (content := initial)

      "content:unset" `isRenderedFrom` (content := unset)

      "content:normal" `isRenderedFrom` (content := normal)

      "content:none" `isRenderedFrom` (content := none)

      "content:contents" `isRenderedFrom` (content := contents)

      "content:url(\"http://www.example.com/test.png\")"
        `isRenderedFrom`
          (content := url "http://www.example.com/test.png")

      "content:linear-gradient(0,#e66465,#9198e5)"
        `isRenderedFrom`
          (content := linearGradient nil $ rgb 230 100 101 /\ rgb 145 152 229)

      "content:url(\"http://www.example.com/test.png\")/\"This is the alt text\""
        `isRenderedFrom`
          ( content :=
              url "http://www.example.com/test.png" /\ "This is the alt text"
          )

      "content:\"prefix\"" `isRenderedFrom` (content := "prefix")
