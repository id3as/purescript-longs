{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "longs"
, backend = "purerl"
, dependencies = [ "prelude", "nullable", "strings", "foreign", "functions", "console", "effect" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs"]
}
