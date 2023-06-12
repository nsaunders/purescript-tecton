# Changelog

Notable changes are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:
- The `LineName` data constructor is now exported, replacing the `lineName` function which is hereby deprecated. nsaunders/purescript-tecton#43

Bugfixes:

Other improvements:

## [0.2.0] - 2023-05-16

Breaking changes:
- _For pseudo-elements only_, the `&:` operator has been replaced by `&::`. Pseudo-classes continue to work with the `&:` operator. nsaunders/purescript-tecton#33
- The `keyframesName` function has been dropped. Just use the `KeyframesName` constructor instead. (nsaunders/purescript-tecton#34)
- The `CustomAttribute` type and `att` constructor function have been removed in favor of the [`AttrName` type from `web-html`](https://pursuit.purescript.org/packages/purescript-web-html/4.1.0/docs/Web.HTML.Common#t:AttrName). nsaunders/purescript-tecton#35
- The `&.` operator (`byClass` function) no longer accepts a string argument. Instead, it requires a [`ClassName`](https://pursuit.purescript.org/packages/purescript-web-html/4.1.0/docs/Web.HTML.Common#t:ClassName). nsaunders/purescript-tecton#35
- The `&#` operator (`byId` function) no longer accepts a string argument. Instead, it requires a value of the newly-added `ElementId` type. nsaunders/purescript-tecton#35, nsaunders/purescript-tecton#37
- The `nth` function has been dropped, replaced by the `#+` and `#-` operators that can be used to construct **a**_n_+**b** formulas. nsaunders/purescript-tecton#36 

New features:
- `box-sizing` property nsaunders/purescript-tecton#31
- `cursor` property nsaunders/purescript-tecton#41
- `word-break` property nsaunders/purescript-tecton#32
- Support for custom pseudo-classes and pseudo-elements via the `PseudoClass` and `PseudoElement` constructors nsaunders/purescript-tecton#38
- A new `unsafeDeclaration` function offers an "escape hatch" for e.g. vendor-prefixed or experimental properties that haven't been added to the library yet. nsaunders/purescript-tecton#40

Bugfixes:
- Fixed the content of the compiler error that results from duplicate properties or descriptors within a single ruleset. Previously all values were incorrectly reported as having the type `CommonKeyword`. nsaunders/purescript-tecton#39

## [0.1.6] - 2022-12-12

New features:
- `:focus-within` pseudo-class nsaunders/purescript-tecton#17
- `appearance` property nsaunders/purescript-tecton#18

Other improvements:
- Examples and tests are now formatted using `purs-tidy`.

## [0.1.5] - 2022-12-05

New features:
- Added the `Declarations` type alias.

## [0.1.4] - 2022-11-29

New features:
- Grid support / new CSS properties nsaunders/purescript-tecton#12
  - `grid-auto-columns`
  - `grid-auto-flow`
  - `grid-auto-rows`
  - `grid-column-end`
  - `grid-column-start`
  - `grid-row-end`
  - `grid-row-start`
  - `grid-template-columns`
  - `grid-template-rows`
- Flexbox properties extended to support additional values defined in
  [Box Alignment Module Level 3](https://www.w3.org/TR/css-align-3)
  - `justify-content`
  - `align-content`
  - `justify-self`
  - `align-self`
  - `justify-items`
  - `align-items`

Other improvements:
- New examples of type safety nsaunders/purescript-tecton#10
  - [`TypeError.SelectorPseudoClassingPseudoElement`](examples/type-errors/SelectorPseudoClassingPseudoElement.purs)
  - [`TypeError.SelectorPseudoElementDescendant`](examples/type-errors/SelectorPseudoElementDescendant.purs)
  - [`TypeError.GridTemplateColumnsAutoRepeatWithFlex`](examples/type-errors/GridTemplateColumnsAutoRepeatWithFlex.purs)
  - [`TypeError.GridTemplateColumnsMultipleAutoRepeat`](examples/type-errors/GridTemplateColumnsMultipleAutoRepeat.purs)
- The [`check-examples`](scripts/check-examples.mjs) script now verifies that each `TypeError.*` example fails
  to compile.

## [0.1.3] - 2022-11-09

Other improvements:
- Performance optimizations nsaunders/purescript-tecton#9
- Dropped `arrays` dependency

## [0.1.2] - 2022-11-06

New features:
- A single property appearing more than once within a ruleset now results in a compiler error. nsaunders/purescript-tecton#7
- A new type alias `CSS` provides a way to annotate style sheet values without using internal types. nsaunders/purescript-tecton#8

## [0.1.1] - 2022-11-03

New features:
- Declarations are now guaranteed to be rendered in the input order. nsaunders/purescript-tecton#6

## [0.1.0] - 2022-10-14

Initial release
