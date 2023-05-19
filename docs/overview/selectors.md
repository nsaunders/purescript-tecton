# Selectors

A selector is a pattern that describes the scope of a rule, i.e. the set of HTML elements to which it should apply. Tecton supports several types of selectors, including element, class, ID, attribute, and pseudo-class selectors. You can combine these to form more advanced selectors or even a selector list.

## Element selectors

An element selector targets all HTML elements of a particular type, such as `div`, `p`, `a`, or `img`. In its most basic form, an element selector simply consists of the target element type, e.g.

```haskell
module Example.StyleSheet where

import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  a ? Rule.do -- Applies to HTML `a` (anchor) elements.
    textDecorationLine := none
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4YZgGcAxilpgpAgCQBKVQB4AfBo0s2HRADNTMeZQEJzlvIdbp2lACq0CnCPRzzazdGt6EX5aDTgUWTh3T28YX39Ao2cTUVkUCywAYRAYWnlBCBF0ixy8gsNkl2j7EBEqk1dakSwAJWh4SLh22CZZQmIyCjgALhG4LJwcBn6ieKHKAF5pEAY4CLgAfm6OrDAQOABaQ7gAQXRMZhgovAOACVcAWQAZOAADFDe4ARQReRJ2OEYLBgDARHhZFg1us4HgYEg8AARSzsFB4Zh1Z7METwEbLER1RgMYAobGjca2CxWOCicTE0kiOCLaExLw+PwBPBYYKhaHrNRwIolGBlfKMGFwAVwABE2nQ9F02nk4BgumlcD0cB5eVI5Eo8oohDgs0Geo1uhl2gA9Mr+IqrQa1UA)

### Universal selector

The _universal selector_ is a special type of element selector that applies to any element. In CSS, it is denoted by the `*` symbol. In Tecton, it is called `universal`. Any selector that does not target another type of element must begin with the universal selector.

> **Note**
> If you have written CSS before, you have almost certainly used the universal selector, perhaps without realizing: It is implicitly included in other types of selectors; for example, `.foo` is actually the same as `*.foo`. For more information, see [Selectors Level 3 (W3C)](https://www.w3.org/TR/selectors-3/#universal-selector).

The following rule uses the universal selector to remove the margin from every element in the webpage:

```haskell
module Example.StyleSheet where

import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  universal ? Rule.do
    margin := nil
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4YZgGcAxilpgpAgCQBKVQB4AfBo0s2HRADNTMeZQEJzlvIdbp2lACq0CnCPRzzazdGt6EX5aDTgUWTh3T28YX39Ao2cTUVkUCywAYRAYWnlBCBF0ixy8gsNkl2j7EBEqk1dakSwAJWh4SLh22CZZQmIyCjgALhG4LJwcBn6ieKHKAF5pEAY4OCLmADc89Kg4AH5ujqwwVfX14CUAc2YRUeWxKCYru9Hx2wsrOFFxBlf7os1tEPF4fH4AngsMFQsD1moNsUMjAyvlGBc4Ai4AAibToei6bTycAwXTYuB6OAwvKkciUfEUQhwWaDOkU3Q47QAemJ-EJXIZZKAA)

## Class selectors

A class selector matches any element whose `class` attribute contains the specified class name. The `&.` operator appends the target class to a selector. For example, the following selector targets all elements with the class name _container_:

```haskell
module Example.StyleSheet where

import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  universal &. ClassName "container" ? Rule.do
    width := auto
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4YZgGcAxilpgpAgCQBKVQB4AfBo0s2HRADNTMeZQEJzlvIdbp2lACq0CnCPRzzazdGt6EX5aDTgUWTh3T28YX39Ao2cTUVkUCywAYRAYWnlBCBF0ixy8gsNkl2j7EBEqk1dakSwAJWh4SLh22CZZQmIyCjgALhG4LJwcBn6ieKHKAF5pEAY4OCLmADc89Kg4ADIsCahI2QA5NHgAInk6vBRmETzruAB+bo6sMFX19apmGA8CRRssUBA8KsGMBHiJRuNbBYrHBROJobC4Is1tEPF4fH4AngsMFQtj1moNsUMjAyvlGH84BS4NdtOh6LptHd+LpXno4CS8qRyJQ2RRCHBZoNhXA+SyAPRcmAcuWinlAA)

## ID selectors

An ID selector targets the element whose `id` attribute is exactly the specified value. The `&#` operator appends the element ID to another selector. The selector in the following example matches the element with ID _appBar_:

```haskell
module Example.StyleSheet where

import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  universal &# ElementId "appBar" ? Rule.do
    height := px 64
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4YZgGcAxilpgpAgCQBKVQB4AfBo0s2HRADNTMeZQEJzlvIdbp2lACq0CnCPRzzazdGt6EX5aDTgUWTh3T28YX39Ao2cTUVkUCywAYRAYWnlBCBF0ixy8gsNkl2j7EBEqk1dakSwAJWh4SLh22CZZQmIyCjgALhG4LJwcBn6ieKHKAF5pEAY4OCLmADc89Kg4ADIAYkRYYBgRPABJMDgAIhR0dAAhJTu4AH5ujqwwVfX1mRmABzEiUEbLdBIOAANgALExgChmCJRuNbBYrHBROIGEiUXBFmtoh4vD4-AE8FhgqFies1BtihkYGV8owAXAGfdtOh6LptPJwDBdO89HAaXlSORKLyKIQ4LNBtK4GK7toAPSC-j89WykVAA)

## Attribute selectors

An attribute selector targets elements based on their attributes or attribute values. It can match on the presence of any HTML attribute such as `href`, `src`, or `alt`. A number of operators can further limit matches to specific attribute values.

### Presence

To match elements based on the presence of an attribute, the attribute is used by itself without any additional operator. For example, the following selector applies to an `input` element with a `required` attribute:

```haskell
module Example.StyleSheet where

import Color (rgb)
import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  input &@ required ? Rule.do
    backgroundColor := rgb 255 153 0
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4YZgGcAxilpgpAgCQBKVQB4AfBo0s2HRADNTMeZQEJzlvIdbp2lACq0CnCPRzzazdGt6EX5aDTgUWTh3T28YX39Ao2cTUVkUCywAYRAYWnlBCBF0ixy8gsNklzgcqHY4AVoAcwAjR2M3exARKpNXLpEsACVoeEi4EdgmWUJiMgo4AC5FmpwcBhmiePnKAF5pEAY4OGYRdAhKADIAATh6AEcIZnowOAB+CdGsMEPj45aUPIANZNWggIpgWr1Rb7ZotOAAJgArEi4ABGJEAZjgAAYmMAUKclitbBYrHBROIGASibsjtEPF4fH4AngsMFQvTjmo4EUSjAyvlGH84Dy4AAibToei6bTycAwXTiuB6O4wEJ5UjkSjSiiEOCbObalW6CXaAD08v4svNuqVQA)

### Exact value 

The `@=` operator can be used to target an element whose attribute value exactly matches the specified value. For example, the following selector matches elements with the attribute `role="button"`:

```haskell
module Example.StyleSheet where

import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  universal &@ role @= "button" ? Rule.do
    fontWeight := bold
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4YZgGcAxilpgpAgCQBKVQB4AfBo0s2HRADNTMeZQEJzlvIdbp2lACq0CnCPRzzazdGt6EX5aDTgUWTh3T28YX39Ao2cTUVkUCywAYRAYWnlBCBF0ixy8gsNkl2j7EBEqk1dakSwAJWh4SLh22CZZQmIyCjgALhG4LJwcBn6ieKHKAF5pEAY4OCLmADc89Kg4ADIAAThaEFg4I+WAIgAjCDw8Ouu4AH5ujqwwVfX10zq8AB1GDMADmJEoI2Wt3OYCYwBQzBEo3GtgsVjgonEDARSLgizW0Q8Xh8fgCeCwwVChPWag2xQyMDK+UYvzgdLg1206Houm08nAMF0Lz0pxgITypHIlB5FEIcFmg2lcFFXIA9AL+Hy1bLhUA)

### Value within list

The `~=` operator can be used to find a matching value within a whitespace-delimited list of words. In the following example, the selector will match an element with an attribute `class="foo bar baz"`. (This is, of course, equivalent to a [class selector](#class-selectors).)

```haskell
module Example.StyleSheet where

import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  universal &@ class' ~= "bar" ? Rule.do
    display := none
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4YZgGcAxilpgpAgCQBKVQB4AfBo0s2HRADNTMeZQEJzlvIdbp2lACq0CnCPRzzazdGt6EX5aDTgUWTh3T28YX39Ao2cTUVkUCywAYRAYWnlBCBF0ixy8gsNkl2j7EBEqk1dakSwAJWh4SLh22CZZQmIyCjgALhG4LJwcBn6ieKHKAF5pEAY4OCLmADc89Kg4ADIAATh5KEjZAHI4AD9lgCIAIyV7uAB+bo6sMFX19ZlZJgUARRssRHVGAxgChmCJRuNbBYrHBROIoTC4Ys1tEPF4fH4AngsMFQtj1moNsUMjAyvlGH84BS4PdtOh6LptPJwDBdK89HASXlSORKGyKIQ4LNBiK4PyWQB6Ln8DnysW8oA)

### Code/subcode

Intended primarily for locale tags, the `|=` operator matches when an attribute value
- exactly matches the specified value or
- begins with the specified value followed by a hyphen.

The selector in the following example will match an `a` element whose `hreflang` attribute is equal to `"en"` or `"en-US"`, but not `"end"`:

```haskell
module Example.StyleSheet where

import Color (rgb)
import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  a &@ hreflang |= "en" ? Rule.do
    color := rgb 255 0 196
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4YZgGcAxilpgpAgCQBKVQB4AfBo0s2HRADNTMeZQEJzlvIdbp2lACq0CnCPRzzazdGt6EX5aDTgUWTh3T28YX39Ao2cTUVkUCywAYRAYWnlBCBF0ixy8gsNklzgcqHY4AVoAcwAjR2M3exARKpNXLpEsACVoeEi4EdgmWUJiMgo4AC5FmpwcBhmiePnKAF5pEAY4CLgAMgABOBJ6UygUESa4AB99gCIYEVe4AH4J0awwIdjsd5CA6rQlvtmi04AAmACs8LgAAY4ABGACcADYmMAUMwREsVrYLFY4KJxAw8QS4LsjtEPF4fH4AngsMFQvTjmo4EUSjAyvlGMC4Dy4K9tOh6LptKD+Lovno4By8qRyJQpRRCHBNnN1XAlRKAPRymAyo2ahVAA)

### Prefix

You can use the `^=` operator to represent an element whose attribute value begins with the specified value. The following example targets insecure links based on their _http://_ prefix:

```haskell
module Example.StyleSheet where

import Color (rgb)
import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  a &@ href ^= "http://" ? Rule.do
    color := rgb 255 0 0
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4YZgGcAxilpgpAgCQBKVQB4AfBo0s2HRADNTMeZQEJzlvIdbp2lACq0CnCPRzzazdGt6EX5aDTgUWTh3T28YX39Ao2cTUVkUCywAYRAYWnlBCBF0ixy8gsNklzgcqHY4AVoAcwAjR2M3exARKpNXLpEsACVoeEi4EdgmWUJiMgo4AC5FmpwcBhmiePnKAF5pEAY4CLgAMgABOBJ6UzgAPX2AIhI8PHRFgHoPx7gAfgnRlgwIdjsd5CA6rQlvtmi04AAmACsiLgAAY0UxgChmCIlitbBYrHBROIGFicXBdkdoh4vD4-AE8FhgqFqcc1HAiiUYGV8oxQXAOXBHtp0PRdNpwfxdD89HAWXlSORKGKKIQ4Js5sq4HKRR8pTAJR9VTKgA)

### Suffix

The `$=` operator matches an element with an attribute value ending in the specified value. This can be used to target a link to a PDF, for instance:

```haskell
module Example.StyleSheet where

import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  a &@ href $= ".pdf" ? Rule.do
    fontWeight := bold
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4YZgGcAxilpgpAgCQBKVQB4AfBo0s2HRADNTMeZQEJzlvIdbp2lACq0CnCPRzzazdGt6EX5aDTgUWTh3T28YX39Ao2cTUVkUCywAYRAYWnlBCBF0ixy8gsNkl2j7EBEqk1dakSwAJWh4SLh22CZZQmIyCjgALhG4LJwcBn6ieKHKAF5pEAY4CLgAMgABOBJ6Uzg1ZYAiLHQwUxO4AH5ujqwwVfX10zq8AHUYZgBzEkoRssAEYgKBgJjAFDMESjca2CxWOCicQMSHQuCLNbRDxeHx+AJ4LDBUJY9ZqOBFEowMr5RgvI7rE7adD0XTaeTgGC6a56ODEvKkciUFkUQhwWaDIVwXlMgD0HP4bNlIu5QA)

### Substring

To target an element whose attribute value simply contains a given substring, use the `*=` operator. The following selector matches any image sourced from Wikimedia:

```haskell
module Example.StyleSheet where

import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  img &@ src *= "wikimedia.org" ? Rule.do
    display := initial
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4YZgGcAxilpgpAgCQBKVQB4AfBo0s2HRADNTMeZQEJzlvIdbp2lACq0CnCPRzzazdGt6EX5aDTgUWTh3T28YX39Ao2cTUVkUCywAYRAYWnlBCBF0ixy8gsNkl2j7EBEqk1dakSwAJWh4SLh22CZZQmIyCjgALhG4LJwcBn6ieKHKAF5pEAY4OFYAczgAMgABOFl8uAAqZYAiKmYAa1YYGRQsdk3zuAB+bo6sMFX19ZlZJgUARRstmGI8MwUFAmMAUODRuNbBYrHBROIGHCEYs1tEPF4fH4AngsMFQrj1mo4EUSjAyvlGH84FS4OdtOh6LptPJwDBdK89HAyXlSORKByKIRDgN5mK4IK2QB6Hn8LmKiX8oA)

## Pseudo-classes

A pseudo-class can be used to style elements based on their state or position in the DOM tree. You can append a pseudo-class to the end of a selector using the `&:` operator. For instance, the `focus` pseudo-class applies a style conditionally when the element has focus:

```haskell
module Example.StyleSheet where

import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  universal &: focus ? Rule.do
    outlineStyle := solid
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4YZgGcAxilpgpAgCQBKVQB4AfBo0s2HRADNTMeZQEJzlvIdbp2lACq0CnCPRzzazdGt6EX5aDTgUWTh3T28YX39Ao2cTUVkUCywAYRAYWnlBCBF0ixy8gsNkl2j7EBEqk1dakSwAJWh4SLh22CZZQmIyCjgALhG4LJwcBn6ieKHKAF5pEAY4OCLmADc89Kg4ADJx0xB5CCiAfm6OrDBV9fWQCDwoZhF4gfgR5dkQV7AmMAUG9RuNbBYrHBROIGECQYs1tEPF4fH4AngsMFQoj1moNsUMjAyvlGA84Hi4AAibToei6bTycAwXSUuB6OBYvKkciUWkUQhwWaDHls3RU7QAekZ-HpEr5LKAA)

### Basic pseudo-classes

The most common pseudo-classes usually consist of a single keyword, such as the following:

- `link` targets an unvisited link, i.e. an `a` element that the user has not yet clicked or visited.
- `visited` targets a visited link, i.e. an `a` element that the user has clicked or visited in the past.
- `hover` matches an element while the user's mouse cursor hovers over it.
- `focus` matches an element when it has keyboard focus. An element typically receives focus through user interaction, such as clicking on an input field.
- `focusWithin` targets an element containing a descendant element that has keyboard focus.
- `active` matches an element while it is being clicked, pressed, or activated.
- `target` targets an element whose ID matches the current location hash. This is often useful with deep-linking in order to draw the user's attention to the requested section of the page.
- `enabled` targets an interactive element (e.g. `input`, `select`, or `button`) that is enabled.
- `disabled` targets an interactive element (e.g. `input`, `select`, or `button`) that is disabled.
- `checked` matches a radio button, checkbox, or option element that is checked or selected.
- `indeterminate` matches a checkbox element in an indeterminate state, i.e. neither checked nor unchecked.
- `root` targets the highest-level element in the document, typically an `html` element.
- `firstChild` targets an element with no preceding siblings.
- `lastChild` targets an element with no subsequent siblings.
- `firstOfType` matches an element with no preceding siblings of the same type.
- `lastOfType` matches an element with no subsequent siblings of the same type.
- `onlyChild` matches an element with no siblings.
- `onlyOfType` matches an element with no siblings of the same type.
- `empty` matches an element with no children or that contains only empty text nodes.

### The `lang` pseudo-class

The `lang` pseudo-class can be used to target an element based on the value of its `lang` attribute (or the value inherited from an ancestor). The `lang` pseudo-class is parameterized by a one- or two-part language tag consisting of an [ISO 639-1 language code](https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes), optionally followed by a hyphen and an [ISO 3166-1 alpha-2](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2) country code.

<!-- TODO finish lang section -->

<!-- TODO not -->

### Nth pseudo-classes

A special group of pseudo-classes allows you to select an element by its position within the parent element. These use a formula of the form **a**_n_+**b** where:

- **a** determines the frequency of matches. For example, **3**_n_ can be interpreted as _every third element_.
- **b** represents an offset, i.e. the index of the first matching element (one-indexed). For example, **2** will match the second element, before the pattern repeats.

These pseudo-classes are
- `nthChild`, which selects elements matching the **a**_n_+**b** formula within their parent;
- `nthLastChild`, which reverses the direction of `nthChild`, counting from the last child element within a container;
- `nthOfType`, which is like a version of `nthChild` that ignores sibling elements of a different type when evaluating the **a**_n_+**b** formula; and
- `nthLastOfType`, which reverses the direction of `nthOfType`, counting from the last child element within a container.

The [:nth Tester (CSS-Tricks)](https://css-tricks.com/examples/nth-child-tester/) offers an interactive way to learn how these pseudo-classes and the **a**_n_+**b** formula work.

Constructing these pseudo-classes in Tecton begins with the formula:

- The `even` function will select even-numbered elements, equivalent to **2**_n_.
- The `odd` function will select odd-numbered elements, equivalent to **2**_n_+**1**.
- The `#+` operator has two parameters **a** and **b**, used to create a **a**_n_+**b** formula, e.g. `2 #+ 1`.
- The `#-` operator has two parameters **a** and **b**, used to create a **a**_n_-**b** formula, e.g. `2 #- 1`.

Apply one of the `nth*` functions listed above to the result to create the pseudo class.

Here is how `nthChild`, for instance, can be used to add zebra-striping to a table:

```haskell
module Example.StyleSheet where

import Color (rgb)
import Tecton
import Tecton.Rule as Rule

styleSheet :: CSS
styleSheet = do
  tr &: nthChild even ? Rule.do
    backgroundColor := rgb 240 240 240
```

[![Open with Try PureScript](https://shields.io/badge/-Open%20in%20Try%20PureScript-303748?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAAArElEQVQ4EeWRzQ6CMBCECSJnwOfiwN2YmCgn49F49cnrzNLdDFhfQDdpd+fbH2hbVf9lKaUaizZ/PTmSByv53I5AO8FjcQgKWqyXFGrYs0nAFEMAn0wEKARIN45ZSzMNfza1bHu4u2gNOzYIuFDMDnKyc73xN2gOdzv51w2YWKbHbzpTn7sfwQAmH0mIuHe98bzYNho1QGKUYr41n6xkg/atYlRfc0e9Svy+eAM93kRyOW/z2AAAAABJRU5ErkJggg==&style=flat)](https://try.purescript.org/?code=LYewJgrgNgpgBAUQB4ENgAdYDoDKAXAT1hwAsYY84B3MgJxgCgGBLDEWygBXqgjHgAUAVQB2zPABo4YZgGcAxilpgpAgCQBKVQB4AfBo0s2HRADNTMeZQEJzlvIdbp2lACq0CnCPRzzazdGt6EX5aDTgUWTh3T28YX39Ao2cTUVkUCywAYRAYWnlBCBF0ixy8gsNklzgcqHY4AVoAcwAjR2M3exARKpNXLpEsACVoeEi4EdgmWUJiMgo4AC5FmpwcBhmiePnKAF5pEAY4ODxaOAAyFZE8EiySZigwOBgANxgROAB+CdGsMEPjscWih5ABrJq0EBFMC1eqLfbNFpwABMABYAAwojFY9FMYAoZgfZZmCxWOCicQMfGEuC7I7RDxeHx+AJ4LDBUL045qOBFEowMr5RiAuA8uAAIm06Houm08nAMF04rgejgHLypHIlGlFEIcE2cy1Kt0Eu0AHp5fxZWadUqgA)

<!-- TODO custom pseudo-classes -->

<!-- TODO pseudo-elements -->

<!-- TODO selector lists -->

## See also

- [Selectors spec](https://github.com/nsaunders/purescript-tecton/tree/master/test/SelectorsSpec.purs)
- [Selectors Level 4 (W3C)](https://www.w3.org/TR/selectors-4/)
