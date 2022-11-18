module Tecton.Rule where

import Prelude hiding (discard)

import Control.Monad.Writer (Writer)
import Prim.Row as Row
import Type.Proxy (Proxy(..))

discard
  :: forall w a b c
   . Monoid w
  => Row.Union a b c
  => Row.Nub c c
  => Writer w (Proxy a)
  -> (Proxy a -> Writer w (Proxy b))
  -> Writer w (Proxy c)
discard wa awb = wa >>= \a -> awb a >>= \_ -> pure Proxy
