module Tecton.Rule where

import Prelude hiding (discard)
import Control.Monad.Writer (Writer)
import Data.Tuple.Nested (type (/\), (/\))

discard
  :: forall w a b
   . Monoid w
  => Writer w a
  -> (a -> Writer w b)
  -> Writer w (a /\ b)
discard wa awb = wa >>= \a -> awb a >>= \b -> pure (a /\ b)
