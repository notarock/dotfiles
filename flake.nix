{
  description = "Notarock's modest flake";
  inputs = {
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    doom-emacs.url = "github:hlissner/doom-emacs/develop";
    doom-emacs.flake = false;
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-discord.url = "github:nixos/nixpkgs/nixos-unstable-small";

    gore.url = "github:motemen/gore/main";
    gore.flake = false;
    gotools.url = "git+https://go.googlesource.com/tools";
    gotools.flake = false;
    gotests.url = "github:cweill/gotests/develop";
    gotests.flake = false;
    gomodifytags.url = "github:fatih/gomodifytags";
    gomodifytags.flake = false;

    stumpwm.url = "github:/stumpwm/stumpwm/master";
    stumpwm.flake = false;
  };

  outputs = inputs@{ self, nixpkgs, home-manager, sops-nix, ... }:
    let
      system = "x86_64-linux";

      nixpkgsPatched = let pkgs = (import nixpkgs { inherit system; });
      in pkgs.stdenv.mkDerivation {
        name = "nixpkgs-patched";
        src = nixpkgs;
        patches = with pkgs;
          [
            # Fix pycurl build
            (fetchpatch { # https://github.com/NixOS/nixpkgs/pull/166335
              url =
                "https://github.com/NixOS/nixpkgs/commit/c270defab79e46b4c98039b09ab6209d1a69ffb3.patch";
              sha256 = "0higphrwvrkxrhq4qg7ip37x5iq64jpyfzw97il5x1zvfpcpwj05";
            })
          ];
        dontFixup = true;
        installPhase = ''
          mv $(realpath .) $out
        '';
      };

      pkgs = import nixpkgsPatched {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          (final: prev: {
            lispPackages = prev.lispPackages // {
              stumpwm = (prev.lispPackages.stumpwm.overrideAttrs (o: rec {
                src = inputs.stumpwm;
                version = "git-2022-04";
              }));
            };
          })
        ];
      };
      mkNixosConfiguration = { hostname }:
        let
          hardwareConfig = ./hosts + "/${hostname}/hardware-configuration.nix";
        in nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = { inherit inputs; };
          modules = [
            hardwareConfig
            ./configuration.nix
            home-manager.nixosModules.home-manager
            { home-manager.extraSpecialArgs = { inherit inputs; }; }
            { nixpkgs.overlays = [ inputs.emacs-overlay.overlay ]; }
            sops-nix.nixosModules.sops
            ./extras/stumpwm-wrapper.nix
          ];
          inherit pkgs;
        };

    in {
      nixosConfigurations = {
        Zonnarth = mkNixosConfiguration { hostname = "Zonnarth"; };
        Kreizemm = mkNixosConfiguration { hostname = "Kreizemm"; };
      };
    };
}
