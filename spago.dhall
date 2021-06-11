{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "debugged"
, dependencies =
  [ "arrays"
  , "bifunctors"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "lists"
  , "ordered-collections"
  , "psci-support"
  , "record"
  , "strings"
  , "tuples"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
