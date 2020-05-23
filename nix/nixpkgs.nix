let
  nixpkgs-src = builtins.fetchTarball {
    name = "nixos-20.03";
    url = "https://github.com/nixos/nixpkgs/archive/6d68b920eb2df73d68a9355f9d572dbf97add5f5.tar.gz";
    sha256 = "0j150qa5v3z9w35079z43q4s9bnrng3sq3l5l2kj8fsfkwb35syx";
  };

in
  import nixpkgs-src
