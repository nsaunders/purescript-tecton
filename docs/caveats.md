Caveats
=======

## Vendor prefixes

You may want to consider applying [Autoprefixer](https://github.com/postcss/autoprefixer) to the CSS output to improve your style sheet's compatibility with older browsers.

## Shorthand

Shorthand syntax is generally avoided, in part because its complex structure is often difficult to represent at the type level, and also because [it might be an anti-pattern](https://csswizardry.com/2016/12/css-shorthand-syntax-considered-an-anti-pattern/).

## Component value ordering

The `&&` and `||` [component value combinators](https://www.w3.org/TR/css-values-4/#component-combinators) are unopinionated about the ordering of component values. Tecton, on the other hand, requires component values to appear in the same order as they are referenced in the relevant specification. In other words:

> [CSS Values and Units Module Level 4](https://www.w3.org/TR/css-values-4/#component-combinators):
> * A double ampersand (&&) separates two or more components, all of which must occur, ~~in any order~~ in the specified order.
> * A double bar (||) separates two or more options: one or more of them must occur, ~~in any order~~ in the specified order.
