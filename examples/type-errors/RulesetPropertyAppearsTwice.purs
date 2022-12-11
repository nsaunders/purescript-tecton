{-

This example fails to compile because the same property appears twice within the
same ruleset. Although in some cases this type of issue is harmless and the last
declaration simply overrides previous ones, it is unintentional and should be
reviewed.

-}

module TypeError.RulesetPropertyAppearsTwice where

import Color (black, white)
import Tecton (CSS, color, universal, (:=), (?))
import Tecton.Rule as Rule

css :: CSS
css =
  universal ? Rule.do
    color := white
    color := black
