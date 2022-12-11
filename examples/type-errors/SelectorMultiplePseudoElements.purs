{-

This example fails to compile because multiple pseudo-elements within a selector
would most likely be unintentional and represent a defect.

However, the Selectors Level 4 specification does include the notion of
"sub-pseudo-elements" here:
https://www.w3.org/TR/selectors-4/#sub-pseudo-elements

For now, real-world use cases and browser support for "sub-pseudo-elements" are
unclear. Google it: https://www.google.com/search?q=%22sub+pseudo+elements%22

-}

module TypeError.SelectorMultiplePseudoElements where

import Tecton
  ( CSS
  , after
  , color
  , currentColor
  , placeholder
  , universal
  , (&:)
  , (:=)
  , (?)
  )
import Tecton.Rule as Rule

css :: CSS
css =
  universal &: placeholder &: after ? Rule.do
    color := currentColor
