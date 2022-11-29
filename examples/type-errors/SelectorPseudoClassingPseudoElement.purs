{-

This example fails to compile because a pseudo-class cannot follow a
pseudo-element; therefore, the use of `:hover` is invalid.

This is contrary to the Selectors Level 4 specification
(https://www.w3.org/TR/selectors-4/#pseudo-element-states) and should be
reviewed in the future. However, as of now, neither Safari 14.1, nor Chrome 107,
nor Firefox 107, supports this notion of "pseudo-classing pseudo-elements".

-}

module TypeError.SelectorPseudoClassingPseudoElement where

import Tecton (CSS, after, hover, textDecorationLine, underline, universal, (&:), (?), (:=))

css :: CSS
css = universal &: after &: hover ? textDecorationLine := underline
