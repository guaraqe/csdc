_: pkgs:

let
  # Contains the auth with supports namedroutes
  servantSrc = pkgs.fetchFromGitHub {
    repo = "servant";
    owner = "haskell-servant";
    rev = "bd9151b9de579e98d14add3328933d155df25fc9";
    sha256 = "sha256-eGZGxKU5mvzDrL2q2omIXzJjbjwvmQzh+eYukYzb3Dc=";
  };

  majority-consensus-src = pkgs.fetchFromGitLab {
    repo = "majorityconsensus";
    owner = "armandguelina";
    rev = "e7ea7b5ea59e4b41ff8aa2a82661de32d7f4cfd9";
    sha256 = "sha256-uGmQQKcDqHV3usOKmpDLHB2GJmIpxVdJXuNI6+CE20c=";
  };

  jre_minimal = pkgs.jre_minimal.override {
    jdk = pkgs.jdk_headless;
  };

  majority-consensus-class = pkgs.stdenv.mkDerivation {
    name = "libfoo";
    src = majority-consensus-src;
    buildPhase = ''
      ${pkgs.openjdk}/bin/javac MajorityConsensus.java
    '';
    installPhase = ''
      mkdir -p $out
      cp *.class $out
    '';
  };

  majority-consensus = pkgs.writeShellScriptBin "majority-consensus" ''
    tmpdir=$(mktemp -d /tmp/majority-consensus-votes.XXXXXX)
    cd $tmpdir
    touch input
    while read line
    do
      echo "$line" >> input
    done
    ${jre_minimal}/bin/java -cp ${majority-consensus-class} MajorityConsensus input 1
  '';

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

  majority-consensus = majority-consensus;
}
