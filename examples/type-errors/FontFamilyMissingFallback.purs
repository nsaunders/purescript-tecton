{-

This example fails to compile because the Fonts Module Level 4 specification
encourages authors "to append a generic font family as a last alternative for
improved robustness" within `font-family` declarations.

See https://www.w3.org/TR/css-fonts-4/#generic-family-value for more
information.

-}

module TypeError.FontFamilyMissingFallback where

import Data.Tuple.Nested ((/\))
import Tecton (CSS, fontFamily, universal, (:=), (?))

css :: CSS
css = universal ? fontFamily := "Roboto" /\ "Arial"
