{-

This example does not compile because pseudo-elements do not exist in the DOM
tree and therefore cannot have descendants.

See https://www.w3.org/TR/selectors-4/#pseudo-element-attachment for more
information.

-}

module TypeError.SelectorLogicAfterPseudoElement where

import Tecton (CSS, after, nil, universal, width, (&:), (|*), (?), (:=))

css :: CSS
css = universal &: after |* universal ? width := nil
