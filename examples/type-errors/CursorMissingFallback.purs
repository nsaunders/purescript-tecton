{-

This example fails to compile because the value of the `cursor` property as
defined in the Basic User Interface Module Level 4 specification requires the
last entry to be a generic/native cursor rather than an image URL.

See https://www.w3.org/TR/css-ui-4/#propdef-cursor for more information.

-}

module TypeError.CursorMissingFallback where

import Data.Tuple.Nested ((/\))
import Tecton (CSS, cursor, universal, url, (:=), (?))

css :: CSS
css = universal ? cursor := url "abc.png" /\ url "xyz.png"