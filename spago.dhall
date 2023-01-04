{ dependencies =
  [ "arrays"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "foreign-object"
  , "form-urlencoded"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "partial"
  , "polyform"
  , "polyform-batteries-core"
  , "prelude"
  , "test-unit"
  , "tuples"
  , "typelevel-prelude"
  , "unfoldable"
  , "validation"
  ]
, license = "BSD-3-Clause"
, name = "polyform-batteries-urlencoded"
, packages = ./packages.dhall
, repository = "https://github.com/purescript-polyform/batteries-urlencoded.git"
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
