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

    stumpwm.url = "github:/stumpwm/stumpwm/master";
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
          (originPkgs.fetchpatch { # https://github.com/NixOS/nixpkgs/pull/178576
            url =
              "https://patch-diff.githubusercontent.com/raw/NixOS/nixpkgs/pull/178576.patch";
            sha256 = "sha256-7soCSNjtXJin6gvX97TFUHfOGTW8+hyOvSqOeH4By+8=";
          })
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
