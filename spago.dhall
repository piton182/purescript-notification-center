{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "argonaut-codecs"
  , "console"
  , "effect"
  , "halogen"
  , "identity"
  , "lcg"
  , "newtype"
  , "psci-support"
  , "quickcheck"
  , "spec"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
