module Test.Util where

import Prelude

import Control.Monad.Writer (Writer)
import Data.List (List)
import Tecton.Internal
  ( class ToVal
  , Declaration'
  , Statement
  , compact
  , renderInline'
  , renderSheet
  , runVal
  , val
  )
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

isRenderedFromInline
  :: forall ps
   . String
  -> Writer (List Declaration') ps
  -> Spec Unit
isRenderedFromInline expected given =
  it ("renders " <> expected) $
    renderInline' compact given `shouldEqual` expected

isRenderedFromSheet
  :: String
  -> Writer (List Statement) Unit
  -> Spec Unit
isRenderedFromSheet expected given =
  it ("renders " <> expected) $
    renderSheet compact given `shouldEqual` expected

isRenderedFromVal
  :: forall a
   . ToVal a
  => String
  -> a
  -> Spec Unit
isRenderedFromVal expected given =
  it ("renders " <> expected) $
    runVal compact (val given) `shouldEqual` expected
