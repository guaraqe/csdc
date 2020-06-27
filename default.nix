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
        buildInputs =
          [
            # Haskell
            localHaskellPackages.ghcid
            # Elm
            elmPackages.elm
            elm2nix
            nodePackages.uglify-js
            # Database
            docker-compose
            postgresql
          ];
      };
in
  if forShell
  then shell
  else packages
