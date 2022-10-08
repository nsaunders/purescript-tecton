module Test.Util where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error)
import Tecton.Internal (class Render, compact, render)
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

isRenderedFrom :: forall a. Render a => String -> a -> Spec Unit
isRenderedFrom expected given =
  it ("renders " <> expected) $ assertRendered expected given

assertRendered
  :: forall m a
   . MonadThrow Error m
 => Render a
 => String
 -> a
 -> m Unit
assertRendered expected given = render compact given `shouldEqual` expected
