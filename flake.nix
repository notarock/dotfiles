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
  };

  outputs = inputs@{ self, nixpkgs, home-manager, nix-doom-emacs, ... }: {
    nixosConfigurations = {
      Zonnarth = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          home-manager.nixosModules.home-manager
          ./configuration.nix
        ];
        specialArgs = {inherit inputs;};
      };
      Kreizemm = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          home-manager.nixosModules.home-manager
          {
            home-manager.users.notarock = {pkgs, ...}: {
              imports = [ nix-doom-emacs.hmModule ];
              programs.doom-emacs = {
                enable = true;
                doomPrivateDir = ./notarock/doom.d;
              };
            };
          }
          ./configuration.nix
        ];
        specialArgs = {inherit inputs;};
      };
    };

    # TODO: Enlever cette bouette
    # packages.x86_64-linux.hello = nixpkgs.legacyPackages.x86_64-linux.hello;
    # defaultPackage.x86_64-linux = self.packages.x86_64-linux.hello;

  };
}
