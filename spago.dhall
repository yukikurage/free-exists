{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "avar"
  , "console"
  , "contravariant"
  , "control"
  , "debug"
  , "distributive"
  , "effect"
  , "either"
  , "exists"
  , "fast-vect"
  , "foldable-traversable"
  , "free"
  , "functors"
  , "identity"
  , "js-timers"
  , "lazy"
  , "leibniz"
  , "lists"
  , "maybe"
  , "naturals"
  , "newtype"
  , "ordered-collections"
  , "pairs"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "qualified-do"
  , "record"
  , "run"
  , "safely"
  , "st"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "type-equality"
  , "typelevel-prelude"
  , "unfoldable"
  , "unsafe-coerce"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
