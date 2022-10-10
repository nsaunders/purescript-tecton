Caveats
=======

## Shorthand

Shorthand syntax is generally avoided, in part because its complex structure is often difficult to represent at the type level, and also because [it might be an anti-pattern](https://csswizardry.com/2016/12/css-shorthand-syntax-considered-an-anti-pattern/).

## Declaration ordering

Because declarations are modeled as records, the order in which they appear in PureScript code may differ from the rendered output.

Navigating this pitfall is as simple as ensuring that declarations are written such that ordering does not matter. For example, shorthand "overrides" (such as overriding a `margin` declaration with `marginLeft`) within a single ruleset should be avoided. If needed, a simple workaround is to split the ruleset into two, since ruleset ordering _is_ preserved.

## Vendor prefixes

You may want to consider applying [Autoprefixer](https://github.com/postcss/autoprefixer) to the CSS output to improve your style sheet's compatibility with older browsers.
