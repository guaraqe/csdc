{ forShell ? false }:

with import ./nix;
with builtins;

let
  # Set of package sources parsed from cabal.project.
  targets =
    import ./nix/cabal.nix;

  # Add local packages to haskellPackages.
  localHaskellPackages =
    haskellPackages.extend (haskell.lib.packageSourceOverrides targets);

  # Set of local packages, built from targets.
  packages =
    mapAttrs (name: _: localHaskellPackages.${name}) targets;

  # Shell for developing the local packages. Includes Haskell dependencies and
  # Elm tools. Elm dependencies are managed by Elm itself, and used with Nix by
  # applying elm2nix.
  shell =
    localHaskellPackages.shellFor
      {
        packages = _: attrValues packages;
        withHoogle = true;
        buildInputs =
          [
            # Haskell
            localHaskellPackages.ghc
            localHaskellPackages.cabal-install
            localHaskellPackages.ghcid
            localHaskellPackages.ormolu
            # Elm
            elmPackages.elm
            elmPackages.elm-analyse
            elmPackages.elm-doc-preview
            elm2nix
            nodePackages.uglify-js
            # Database
            docker-compose
            postgresql
            # Deployment
            flyctl
            # IPFS
            ipfs
            # Majority Judgement
            majority-judgement
          ];
      };
in
  if forShell
  then shell
  else packages
