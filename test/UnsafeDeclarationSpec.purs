module Test.UnsafeDeclarationSpec where

import Prelude

import Tecton
  ( KeyframesName(..)
  , keyframes
  , pct
  , universal
  , unsafeDeclaration
  , (?)
  )
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFromSheet)

spec :: Spec Unit
spec = do

  let isRenderedFrom = isRenderedFromSheet

  describe "unsafeDeclaration" do

    "*{-webkit-text-stroke-width:thin}"
      `isRenderedFrom` do
        universal ?
          unsafeDeclaration "-webkit-text-stroke-width" "thin"

    "@keyframes foo{0%{-moz-opacity:0}100%{-moz-opacity:1}}"
      `isRenderedFrom` do
        keyframes (KeyframesName "foo") ? do
          pct 0 ?
            unsafeDeclaration "-moz-opacity" "0"
          pct 100 ?
            unsafeDeclaration "-moz-opacity" "1"