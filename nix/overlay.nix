_: pkgs:

let
  # Contains the auth with supports namedroutes
  servantSrc = pkgs.fetchFromGitHub {
    repo = "servant";
    owner = "haskell-servant";
    rev = "bd9151b9de579e98d14add3328933d155df25fc9";
    sha256 = "sha256-eGZGxKU5mvzDrL2q2omIXzJjbjwvmQzh+eYukYzb3Dc=";
  };

  overrides = _: hspkgs: with pkgs.haskell.lib;
    {
      ipfs = hspkgs.callCabal2nix "ipfs" (pkgs.fetchFromGitHub {
        repo = "ipfs-haskell";
        owner = "guaraqe";
        rev = "ad21c0815818f9c4e0113bcaef67f026adc6079c";
        sha256 = "sha256-4FyHJ5o/aRzdewy+2RfilwzqicCyebfoql5e/2/WjIk=";
      }) {};

      servant-auth-server = doJailbreak (hspkgs.callCabal2nix "servant-auth-server" "${servantSrc}/servant-auth/servant-auth-server" {});

    };
in

{
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides =
      pkgs.lib.composeExtensions
        (old.overrides or (_: _: {}))
        overrides;
  });
}
