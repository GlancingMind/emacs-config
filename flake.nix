{
  description = "My emacs testing setup";

  #nixConfig = {
  #  extra-substituters = [ "https://nix-community.cachix.org" ];
  #  trusted-public-keys = [
  #    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
  #  ];
  #};

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, flake-utils, emacs-overlay }:
  flake-utils.lib.eachDefaultSystem (system: let
    pkgs = import nixpkgs {
      inherit system;
      overlays = [ emacs-overlay.overlay ];
    };
  in {
    apps = rec {
      emacs = flake-utils.lib.mkApp {
        drv = pkgs.callPackage ./emacs {};
        name = "emacs";
      };
      default = emacs;
    };
  });
}
