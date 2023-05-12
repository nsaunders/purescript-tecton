{ name = "tecton"
, license = "MIT"
, repository = "https://github.com/nsaunders/purescript-tecton"
, dependencies =
  [ "colors"
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
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
