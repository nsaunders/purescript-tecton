{-

This example fails to compile because the gradient `<color-stop-list>` syntax
requires at minimum two color stops.

See https://www.w3.org/TR/css-images-3/#typedef-color-stop-list for more
information.

-}

module TypeError.GradientInsufficientColorStops where

import Color (black, white)
import Tecton (CSS, backgroundImage, deg, linearGradient, universal, (?), (:=))

css :: CSS
css = universal ? backgroundImage := linearGradient (deg 180) black
