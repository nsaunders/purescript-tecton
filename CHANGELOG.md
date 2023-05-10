# Changelog

Notable changes are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:
- _For pseudo-elements only_, the `&:` operator has bee replaced by `&::`. Pseudo-classes continue to work with the `&:` operator. (nsaunders/purescript-tecton#33)

For example:

**Before**
```purescript
universal &: after ? content := ""
```

**After**
```purescript
universal &:: after ? content := ""
```

New features:
- `box-sizing` property nsaunders/purescript-tecton#31
- `word-break` property nsaunders/purescript-tecton#32

Bugfixes:

Other improvements:

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
