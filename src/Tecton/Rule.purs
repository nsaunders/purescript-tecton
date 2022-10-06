module Tecton.Rule where

import Prelude hiding (discard)
import Record.Builder (Builder)

discard :: forall a b c. Builder a b -> (Unit -> Builder b c) -> Builder a c
discard a k = a >>> k unit
