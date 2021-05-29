{
  description = "A very basic flake";
  inputs = {
    # nixos.url = "nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/master";
    doom-emacs.url = "github:hlissner/doom-emacs/develop";
    doom-emacs.flake = false;
    nix-doom-emacs.url = "github:vlaci/nix-doom-emacs";
    nix-doom-emacs.inputs.doom-emacs.follows = "doom-emacs";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs-discord.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = inputs@{ self, nixpkgs, home-manager, nix-doom-emacs, ... }:
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
            {
              home-manager.users.notarock = { pkgs, ... }: {
                imports = [ nix-doom-emacs.hmModule ];
              };
            }
            {
              home-manager.extraSpecialArgs = {
                inherit inputs;
              };
            }
          ];
        };
    in {
      nixosConfigurations = {
        Zonnarth = mkNixosConfiguration { hostname = "Zonnarth"; };
        Kreizemm = mkNixosConfiguration { hostname = "Kreizemm"; };
      };
    };
}
