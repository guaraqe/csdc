{ pkgs }:

with pkgs;

let
  mkDerivation =
    { srcs ? ./elm-srcs.nix
    , src
    , name
    , srcdir ? "./src"
    , registryDat ? ./registry.dat
    , outputJavaScript ? false
    }:
    stdenv.mkDerivation {
      inherit name src;

      buildInputs = [ elmPackages.elm nodePackages.uglify-js ]
        ++ lib.optional outputJavaScript nodePackages_10_x.uglify-js;

      buildPhase = pkgs.elmPackages.fetchElmDeps {
        elmPackages = import srcs;
        elmVersion = "0.19.1";
        inherit registryDat;
      };

      installPhase = ''
        mkdir -p $out
      	elm make --optimize ${srcdir}/Main.elm --output app.big.js
       	uglifyjs app.big.js \
       	  --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
       		| uglifyjs --mangle > app.js
        cp app.js $out
        cp -r ${../www/css} $out/css
        cp ${../www/index.html} $out/index.html
        cp ${../www/favicon.ico} $out/favicon.ico
      '';
    };
in mkDerivation {
  name = "csdc-gui-0.1.0";
  srcs = ./elm-srcs.nix;
  src = ./.;
  srcdir = "./src";
  outputJavaScript = false;
}

