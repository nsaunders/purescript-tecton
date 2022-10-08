Caveats
=======

> ⚠️ **NOTE**: This document is a work in progress.

## Shorthand
Shorthand syntax is generally avoided, in part because its complexity is often difficult to represent at the type level, and also because [it might be an anti-pattern](https://csswizardry.com/2016/12/css-shorthand-syntax-considered-an-anti-pattern/).

## Declaration ordering
The order in which declarations appear in PureScript code may not correspond to their rendering order due to their representation in records. Navigating this pitfall is as simple as ensuring that declarations are written such that ordering does not matter. For example, shorthand "overrides" (such as overriding a `margin` declaration with `marginLeft`) within a single ruleset should be avoided. If needed, a simple workaround is to split the ruleset into two, since ruleset ordering _is_ preserved.
