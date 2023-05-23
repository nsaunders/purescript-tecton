# Animations

CSS animations are a powerful tool for adding motion and interactivity to a webpage. They allow you to animate various properties of HTML elements, such as position, size, color, and opacity.

## Defining keyframes

Animations are defined using keyframes, which specify the intermediate states of an element during the animation. Keyframe rules define the stages of an animation in terms of percent progress. Each rule indicates the value of a given CSS property at that point in the animation.

For example, the following keyframes transition the background and foreground colors between black and white:

```haskell
module Example.StyleSheet where

import Color (black, white)
import Data.Tuple.Nested ((/\))
import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  keyframes (KeyframesName "black-and-white") ? do
    pct 0 /\ pct 100 ? Rule.do
      backgroundColor := black
      color := white
    pct 50 ? Rule.do
      backgroundColor := white
      color := black
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4YZgGcAxilpgpAgCQBKVQB4AfBo0s2HRADNTMeZQEJzlvIdbp2lACq0CnCPRzzazdGt6EX5aDTgUWTh3T28YX39Ao2cTUVkUCywAYRAYWnlBCBF0ixy8gsNklzgcqHY4AQAjKBR5AGspGnEYR2NKABEUPBQsVwhMGCwAORhZPBgwBoEAegAdAyqTV3sQEU23HZEsACVoeEi4U9gmOaJ4sgo4AC4nmpwcBlviB8oAXmkQAw4HA2jACKZaGhZg0ANJgiFQ2RTKFwABEzVabQAtCgQliuvNUeEAPwAoHAuDoKxwAAMcDWlOpAEYaXTSVdJmBARSKY1MQBzWggIpgWr1J7-DHtck8+QgOq0Z7-AmMHlUygAVjZlzOWC5Mt5AqFIrFiol1BI3QNwLlCqVcClbSYwBQzBEz1etgs1NE4gYLrdcF+5JiXh8fgCeCwwVCBrUcCKJRgZXyqop8bR2nQ9F02jl-F0qLgejgMbypHIlGzFEIcC+90rxd0meW+ZgueW1cLQA)

## Creating animation

Animating elements requires at minimum a keyframe animation name and duration, for example:

```haskell
module Example.StyleSheet where

import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  universal ? Rule.do
    animationName := KeyframesName "black-and-white"
    animationDuration := ms 150
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4YZgGcAxilpgpAgCQBKVQB4AfBo0s2HRADNTMeZQEJzlvIdbp2lACq0CnCPRzzazdGt6EX5aDTgUWTh3T28YX39Ao2cTUVkUCywAYRAYWnlBCBF0ixy8gsNkl2j7EBEqk1dakSwAJWh4SLh22CZZQmIyCjgALhG4LJwcBn6ieKHKAF5pEAY4OCLmADc89Kg4AH5ujqwwVfX1lDFgFDxmOoA5NHgR5YBpGAJTWmfZJ+B4AAiABGUBQ8gA1gBaK5gKE0cQwQFrC5XVi3e4iAAi3gxdVGy2AUQAjABWAAMTBuzBEo3GtgsVjgonEDGptMWKJiXh8fgCeCwwVCKPWag2xQyMDK+UYFzgYrggO06Houm08nAMF0gLgejgQrypHIlBVFEIcFmg2Nut0iu0AHoNfw1fbTdqgA)

A range of properties can be used to control animations:

* `animationName` specifies the name of the keyframe animation to apply.
* `animationDuration` sets the duration of the animation.
* `animationTimingFunction` defines the timing curve for the animation (e.g. `linear`, `easeIn`, or `easeOut`).
* `animationDelay` adds a delay before the animation starts.
* `animationIterationCount` sets the number of times the animation should repeat.
* `animationDirection` controls whether the animation should play in the `normal` or `reverse` directions, or should `alternate` (or `alternateReverse`).
* `animationFillMode` specifies how the element should be styled before and after the animation (e.g. `none`, `forwards`, `backwards` or `both`).

Each of these properties also supports a list of animations by using Tecton's list syntax based on nested tuples, e.g. `animationDuration := ms 150 /\ ms 250 /\ ms 75`.

## Transitions

Keyframe animations provide flexibility and control for complex, multi-step animations. For basic, one-off property changes triggered by events, transitions offer a lightweight approach with no need for a `keyframes` at-rule. Instead, you simply leverage the following properties:

* `transitionProperty` specifies the property to be transitioned.
* `transitionDuration` specifies the length of time over which the transition should occur.
* `transitionTimingFunction` defines the timing function used for the transition. It determines how intermediate property values are calculated over time.
* `transitionDelay` sets a delay before the transition starts.

Like animation properties, each of the transition properties supports a list of values, e.g. `transitionProperty := width /\ height /\ padding`.

For example, the following rulesets transition the background and foreground colors over a 150-millisecond period on hover:

```haskell
module Example.StyleSheet where

import Color (black, white)
import Data.Tuple.Nested ((/\))
import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  let transitionDemo = ClassName "transition-demo"
  universal &. transitionDemo ? Rule.do
    transitionProperty := backgroundColor /\ color
    transitionDuration := ms 150
    transitionTimingFunction := ease
    backgroundColor := white
    color := black
  universal &. transitionDemo &: hover ? Rule.do
    backgroundColor := black
    color := white
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4YZgGcAxilpgpAgCQBKVQB4AfBo0s2HRADNTMeZQEJzlvIdbp2lACq0CnCPRzzazdGt6EX5aDTgUWTh3T28YX39Ao2cTUVkUCywAYRAYWnlBCBF0ixy8gsNklzgcqHY4AQAjKBR5AGspGnEYR2NKABEUPBQsVwhMGCwAORhZPBgwBoEAegAdAyqTV3sQEU23HZEsACVoeEi4U9gmOaJ4sgo4AC4nmpwcBlviB8oAXmkQAw4HBYJQ8LQUMVxMxdv0YKA4P8si1ZLIpmh4AAicGQ2TQ3YAWn4oExQLgRWYADc8ukoHAAGRYOA4qF4GEiOEIgD8lzOWDAgOBwJZeLZu24IHQeUIz3+jVabQA5rQQEUwLV6ms4PIQHVaGThRDWez+t4huzZXBgFEAIwAVgADAbmUbRezXKxmCJFQAxIpWC1Pf4wSKMIVweXtZWqkIa2iWrrzZ06vWW5oKskU6m0WkMpki-Ec+EgBmvEggbNwHlXSYC52RpUqtVxtMtdrJ3X1IPUEjdJjAFBe56vWwWKxwUTiBgDoe-MkxLw+PwBPBYYKhZ1qcnFDIwMr5MNCrdwTHadD0XTaHX8XSYuB6ODrvKkciUc8UGVfe6v++6E-aZZrxgS9lnfW8gA)

## See also

* [Animations spec](https://github.com/nsaunders/purescript-tecton/tree/master/test/AnimationsSpec.purs)
* [CSS Animations Level 1 (W3C)](https://www.w3.org/TR/css-animations-1/#animation-fill-mode)