{
  description = "My emacs testing setup";

  nixConfig = {
    extra-substituters = [ "https://nix-community.cachix.org" ];
    trusted-public-keys = [ "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=" ];
  };

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/af50806f7c6ab40df3e6b239099e8f8385f6c78b";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay/a9c2a436757f09abc4c7bc0abc4d2529b312e42b";
    };
  };

  outputs = { self, nixpkgs, flake-utils, emacs-overlay }:
  flake-utils.lib.eachDefaultSystem (system: let
    pkgs = import nixpkgs {
      inherit system;
      overlays = [ emacs-overlay.overlay ];
    };
  in rec {
    packages = {
      emacs = pkgs.callPackage ./emacs {};
    };

    apps = rec {
      emacs = flake-utils.lib.mkApp {
        drv = packages.emacs;
        name = "emacs";
      };
      default = emacs;
    };

    devShells = {
      default = pkgs.callPackage ./shell.nix {};
    };
  });
}
