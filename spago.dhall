{ name = "tecton"
, license = "MIT"
, repository = "https://github.com/nsaunders/purescript-tecton"
, dependencies =
  [ "arrays"
  , "colors"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "numbers"
  , "prelude"
  , "record"
  , "strings"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
