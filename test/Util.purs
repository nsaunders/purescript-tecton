module Test.Util where

import Prelude

import PSCSS (class Render, compact, render)
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

renders :: forall a. Render a => a -> String -> Spec Unit
renders given expected =
  it ("renders " <> expected) do
    render compact given `shouldEqual` expected
