let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/mkPackage.dhall
        sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let js-uri =
      mkPackage
        [ "assert", "effect", "functions", "maybe", "prelude" ]
        "https://github.com/srghma/purescript-js-uri.git"
        "d25d83390ba9cf948f46695b55a5511895b0068c"

let polyform =
      mkPackage
        [ "newtype"
        , "ordered-collections"
        , "variant"
        , "profunctor"
        , "invariant"
        , "foreign-object"
        , "run"
        , "transformers"
        , "validation"
        , "foreign"
        ]
        "https://github.com/purescript-polyform/polyform.git"
        "master"

let polyform-batteries-core =
      mkPackage
        [ "affjax"
        , "argonaut"
        , "debug"
        , "decimals"
        , "filterable"
        , "numbers"
        , "polyform"
        , "prelude"
        , "record-extra"
        , "test-unit"
        ]
        "https://github.com/purescript-polyform/batteries-core.git"
        "master"

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20211030/packages.dhall
        sha256:5cd7c5696feea3d3f84505d311348b9e90a76c4ce3684930a0ff29606d2d816c

in  upstream
  with js-uri = js-uri
  with polyform = polyform
  with polyform-batteries-core = polyform-batteries-core
