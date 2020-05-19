let
  erlangReleases = builtins.fetchTarball https://github.com/nixerl/nixpkgs-nixerl/archive/v1.0.4-devel.tar.gz;

  pinnedNix =
    builtins.fetchGit {
      name = "nixpkgs-pinned";
      url = "https://github.com/NixOS/nixpkgs.git";
      rev = "cc6cf0a96a627e678ffc996a8f9d1416200d6c81";
    };

  purerlReleases =
    builtins.fetchGit {
      url = "https://github.com/purerl/nixpkgs-purerl.git";
      ref = "master";
      rev = "5da0a433bcefe607e0bd182b79b220af980a4c78";
    };

  id3asPackages =
    builtins.fetchGit {
      name = "id3as-packages";
      url = "git@github.com:id3as/nixpkgs-private.git";
      rev = "485fcb5e2dccbf2fa3d43fb6c2c45bb68babd601";
      ref = "v2";
    };

  nixpkgs =
    import pinnedNix {
      overlays = [
        (import erlangReleases)
        (import purerlReleases)
        (import id3asPackages)
      ];
    };

in

with nixpkgs;

mkShell {
  buildInputs = with pkgs; [

    # Bash on macOS is ancient
    bash

    nixerl.erlang-22-3.erlang

    # Purescript - we use a specific version rather than
    # whatever the latest is exposed via nixpkgs
    id3as.purescript-0-13-6

    # Purescript extras
    id3as.spago-0-12-1-0
    id3as.dhall-json-1-5-0

    # Purerl backend for purescript
    purerl.purerl-0-0-5

    # Needed by something purescript-y - hopefully A/S can pinpoint what...
    jq
    serfdom

  ];
}
