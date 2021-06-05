{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = [ self.overlay ];
        config = { allowBroken = true; };
      };
    in with pkgs; {
      overlay = self: super:
        let
          hpkgs = super.haskellPackages.override {
            overrides = hself: hsuper: {
              pixiv = super.haskell.lib.doJailbreak hsuper.pixiv;
            };
          };
          ircbot = hpkgs.callCabal2nix "ircbot" ./. { };
        in with super;
        with haskell.lib; {
          inherit ircbot;
          ircbot-dev =
            addBuildTools ircbot [ haskell-language-server cabal-install ];
        };
      defaultPackage.x86_64-linux = ircbot;
      devShell.x86_64-linux = ircbot-dev.envFunc { };
    };
}
