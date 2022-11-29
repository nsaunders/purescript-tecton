{-

This example fails to compile because only certain properties are animatable
and therefore compatible with `transition-property`; and `align-content` is not
among them.

See https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_animated_properties
for more information.

-}

module TypeError.TransitionPropertyNotAnimatable where

import Tecton (CSS, alignContent, transitionProperty, universal, (?), (:=))

css :: CSS
css = universal ? transitionProperty := alignContent
