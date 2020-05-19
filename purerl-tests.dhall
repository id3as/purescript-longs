let conf = ./spago.dhall

in    conf
    ⫽ { sources =
          conf.sources # [ "testPurerl/**/*.purs" ]
      , dependencies =
          conf.dependencies # [ "assert", "console", "erl-test-eunit", "proxy" ]
      }
