let upstream =
      https://raw.githubusercontent.com/purerl/package-sets/erl-0.13.5-20191204/src/packages.dhall sha256:661c8af76281defa0f551d5d27436590139c2b1c2f9405217e2080d89c7a51b9

let overrides =
      { erl-test-eunit =
          { dependencies =
              [ "assert"
              , "console"
              , "debug"
              , "erl-lists"
              , "erl-tuples"
              , "foreign"
              , "free"
              , "prelude"
              , "psci-support"
              ]
          , repo = "ssh://git@github.com/id3as/purescript-erl-test-eunit.git"
          , version = "62fd45d70da3e0bc32b4f86fb03cc9f892de028a"
          }
      }

let additions = {=}

in  upstream // overrides // additions
