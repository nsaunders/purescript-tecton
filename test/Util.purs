module Test.Util where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Maybe (Maybe(..))
import Effect.Exception (Error)
import PSCSS (class Render, compact, render)
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

assertRenderedGuarded
  :: forall x a m
   . Render a
  => MonadThrow Error m
  => String
  -> String
  -> ((x -> Maybe x) -> a)
  -> m Unit
assertRenderedGuarded onFail onPass mk = do
  assertRendered onFail $ mk \x -> guard false *> Just x
  assertRendered onPass $ mk \x -> guard true *> Just x
