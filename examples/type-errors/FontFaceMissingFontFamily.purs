{-

This example fails to compile because a `@font-face` rule must include a
`font-family` descriptor.

See https://www.w3.org/TR/css-fonts-4/#font-family-desc for more information.

-}

module TypeError.FontFaceMissingFontFamily where

import Tecton (CSS, fontFace, fontFamily, src, url, (:=), (?))
import Tecton.Rule as Rule

css :: CSS
css =
  fontFace ? Rule.do
    src := url "foo.woff"
