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
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-21.11";
    nixpkgs-small.url = "github:nixos/nixpkgs/nixos-unstable-small";

    gotools.url = "git+https://go.googlesource.com/tools";
    gotools.flake = false;

    gore.url = "github:motemen/gore/main";
    gore.flake = false;
    gotests.url = "github:cweill/gotests/develop";
    gotests.flake = false;
    gomodifytags.url = "github:fatih/gomodifytags";
    gomodifytags.flake = false;

    stumpwm.url =
      "github:/stumpwm/stumpwm/fff2508fd54b4035b0d80bafbd75a13f1756130f";
    stumpwm.flake = false;
  };

  outputs = inputs@{ self, nixpkgs, home-manager, sops-nix, ... }:
    let
      system = "x86_64-linux";
      nixpkgsPatched = let originPkgs = (import nixpkgs { inherit system; });
      in originPkgs.applyPatches {
        name = "nixpkgs-patched";
        src = nixpkgs;
        patches = [
          # (pkgs.fetchpatch { # https://github.com/NixOS/nixpkgs/pull/#####
          #   url = "https://patch-diff.githubusercontent.com/raw/NixOS/nixpkgs/pull/#####.patch";
          #   sha256 = "sha256-someshaxd";
          # })
          #(originPkgs.fetchpatch { # https://github.com/NixOS/nixpkgs/pull/216051
          #  url =
          #    "https://patch-diff.githubusercontent.com/raw/NixOS/nixpkgs/pull/216051.patch";
          #  sha256 = "sha256-aVjt5MZ+lYhKXfFhNHSP/PunhPILjw70mfdTynhE1yc=";
          #})
        ];
      };

      pkgs = import nixpkgsPatched {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          (final: prev: {
            # See: https://www.etsmtl.ca/en/studies/ChemiNot
            cheminot-ets = prev.makeDesktopItem {
              name = "ChemiNot";
              exec = prev.writeShellScript "cheminot" ''
                ${prev.icedtea_web}/bin/javaws <(curl 'https://cheminotjws.etsmtl.ca/ChemiNot.jnlp')
              '';
              comment =
                "ChemiNot is an integrated consultation and registration system for ÉTS students";
              desktopName = "ChemiNot";
            };
          })
          (final: prev: {
            lispPackages = prev.lispPackages // {
              stumpwm = (prev.lispPackages.stumpwm.overrideAttrs (o: rec {
                src = inputs.stumpwm;
                version = "22.11";
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
