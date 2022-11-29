{-

This example fails to compile because only certain properties are animatable
and therefore compatible with `@keyframes` animations; and `content` is not
among them.

See https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_animated_properties
for more information.

-}

module TypeError.KeyframesNonAnimatableProperty where

import Prelude
import Tecton (CSS, content, keyframes, keyframesName, nil, pct, (:=), (?))

css :: CSS
css = do
  keyframes (keyframesName "foo") ? do
    pct 0 ?
      content := ""
    pct 100 ?
      content := "hello"
