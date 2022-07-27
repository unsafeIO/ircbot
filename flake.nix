{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.pixiv = {
    url = "github:The-closed-eye-of-love/pixiv";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, pixiv }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        };
      in with pkgs; {
        packages.default = ircbot;
        devShells.default = ircbot-dev.envFunc { };
      }) // {
        overlays.default = final: prev:
          let
            hpkgs = prev.haskellPackages.override {
              overrides = hself: hsuper: {
                inherit (pixiv.overlays.default final prev) pixiv;
              };
            };
            ircbot = hpkgs.callCabal2nix "ircbot" ./. { };
          in with prev;
          with haskell.lib; {
            ircbot = let
              unwrapped = justStaticExecutables (overrideCabal ircbot (drv: {
                postPatch = ''
                  substituteInPlace app/Eval.hs \
                    --replace 'dsl/PQ.hs' '${drv.src}/dsl/PQ.hs'
                '';
              }));
              runtimeGHC = haskellPackages.ghcWithPackages
                (p: with p; [ pixiv.packages.${system}.default microlens ]);
            in stdenv.mkDerivation {
              name = "ircbot-wrapped";
              buildInputs = [ runtimeGHC unwrapped makeWrapper ffmpeg ];
              dontUnpack = true;
              dontBuild = true;
              installPhase = ''
                mkdir -p $out/bin
                ln -s ${unwrapped}/bin/ircbot $out/bin/ircbot
                wrapProgram $out/bin/ircbot \
                  --set GHC_LIB_DIR "${runtimeGHC}/lib/ghc-${runtimeGHC.version}" \
                  --set GHC_BIN_DIR "${runtimeGHC}/bin" \
                  --set GHC_PACKAGE_PATH "${runtimeGHC}/lib/ghc-${runtimeGHC.version}/package.conf.d" \
                  --prefix PATH : ${ffmpeg}/bin
              '';
            };
            ircbot-dev =
              addBuildTools ircbot [ haskell-language-server cabal-install ];
          };
      };
}
