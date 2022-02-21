{
  description = "Notarock's modest flake";
  inputs = {
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    doom-emacs.url = "github:hlissner/doom-emacs/develop";
    doom-emacs.flake = false;
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable-small";
    nixpkgs-discord.url = "github:nixos/nixpkgs/nixos-unstable";

    gore.url = "github:motemen/gore/main";
    gore.flake = false;
    gotools.url = "git+https://go.googlesource.com/tools";
    gotools.flake = false;
    gotests.url = "github:cweill/gotests/develop";
    gotests.flake = false;
    gomodifytags.url = "github:fatih/gomodifytags";
    gomodifytags.flake = false;
  };

  outputs = inputs@{ self, nixpkgs, home-manager, sops-nix, ... }:
    let
      mkNixosConfiguration = { hostname }:
        let
          hardwareConfig = ./hosts + "/${hostname}/hardware-configuration.nix";
        in nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs; };
          system = "x86_64-linux";
          modules = [
            hardwareConfig
            ./configuration.nix
            home-manager.nixosModules.home-manager
            { home-manager.extraSpecialArgs = { inherit inputs; }; }
            { nixpkgs.overlays = [ inputs.emacs-overlay.overlay ]; }
            sops-nix.nixosModules.sops
          ];
        };
    in {
      nixosConfigurations = {
        Zonnarth = mkNixosConfiguration { hostname = "Zonnarth"; };
        Kreizemm = mkNixosConfiguration { hostname = "Kreizemm"; };
      };
    };
}
