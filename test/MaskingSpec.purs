-- https://www.w3.org/TR/css-masking-1/

module Test.MaskingSpec where

import Prelude

import Color (rgb)
import Data.Tuple.Nested ((/\))
import Tecton
  ( inherit
  , initial
  , linearGradient
  , maskImage
  , nil
  , none
  , unset
  , url
  , (:=)
  )
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFromInline)

spec :: Spec Unit
spec = do

  let isRenderedFrom = isRenderedFromInline

  describe "Masking Module" do

    describe "mask-image property" do

      "mask-image:inherit" `isRenderedFrom` (maskImage := inherit)

      "mask-image:initial" `isRenderedFrom` (maskImage := initial)

      "mask-image:unset" `isRenderedFrom` (maskImage := unset)

      "mask-image:none" `isRenderedFrom` (maskImage := none)

      "mask-image:url(\"https://example.com/xyz.svg\")"
        `isRenderedFrom`
          (maskImage := url "https://example.com/xyz.svg")

      "mask-image:linear-gradient(0,#0000ff,#ff0000)"
        `isRenderedFrom`
          (maskImage := linearGradient nil $ rgb 0 0 255 /\ rgb 255 0 0)

      "mask-image:linear-gradient(0,#0000ff,#ff0000),none,url(\"foo.svg\")"
        `isRenderedFrom`
          ( maskImage :=
              linearGradient nil (rgb 0 0 255 /\ rgb 255 0 0)
              /\ none
              /\ url "foo.svg"
          )
