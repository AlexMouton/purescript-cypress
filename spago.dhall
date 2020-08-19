{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "cypress"
, dependencies =
  [ "console", "effect", "foreign", "psci-support", "transformers", "jquery" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, repository = "https://github.com/alexmouton/purescript-cypress"
, license = "MIT"
}
