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
        packages.pq =
          haskellPackages.ghcWithPackages (p: [ pixiv p.microlens ]);
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
            inherit ircbot;
            inherit (hpkgs) pixiv;
            ircbot-dev =
              addBuildTools ircbot [ haskell-language-server cabal-install ];
          };
      };
}
