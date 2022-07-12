module Test.Util where

import Prelude

import PSCSS (class Render, compact, render)
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

isRenderedFrom :: forall a. Render a => String -> a -> Spec Unit
isRenderedFrom expected given =
  it ("renders " <> expected) $ render compact given `shouldEqual` expected
