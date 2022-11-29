{-

This example fails to compile because a `@font-face` rule must include a `src`
descriptor.

See https://www.w3.org/TR/css-fonts-4/#src-desc for more information.

-}

module TypeError.FontFaceMissingSrc where

import Tecton (CSS, fontFace, fontFamily, (:=), (?))

css :: CSS
css = fontFace ? fontFamily := "Roboto"
