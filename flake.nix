{
  description = "Notarock's modest flake";
  inputs = {
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    doom-emacs.url = "github:doomemacs/doomemacs/master";
    doom-emacs.flake = false;
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-21.11";
    nixpkgs-small.url = "github:nixos/nixpkgs/nixos-unstable-small";

    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    gotools.url = "git+https://go.googlesource.com/tools";
    gotools.flake = false;

    gore.url = "github:motemen/gore/main";
    gore.flake = false;
    gotests.url = "github:cweill/gotests/develop";
    gotests.flake = false;
    gomodifytags.url = "github:fatih/gomodifytags";
    gomodifytags.flake = false;

  };

  outputs = inputs@{ self, nixpkgs, home-manager, sops-nix, darwin, ... }:
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

      # Patched nixpkgs with add-ons
      myPkgs = import nixpkgsPatched {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          (final: prev: {
            # See: https://www.etsmtl.ca/en/studies/ChemiNot
            # Stolen from berbiche :trollface:
            cheminot-ets = prev.makeDesktopItem {
              name = "ChemiNot";
              exec = prev.writeShellScript "cheminot" ''
                ${prev.icedtea_web}/bin/javaws <(curl 'https://cheminotjws.etsmtl.ca/ChemiNot.jnlp')
              '';
              comment =
                "ChemiNot is an integrated consultation and registration system for ÉTS dentues";
              desktopName = "ChemiNot";
            };
          })
        ];
      };

      mkNixosConfiguration = { hostname, username }:
        let
          hardwareConfig = ./hosts + "/${hostname}/hardware-configuration.nix";
        in nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = { inherit inputs; };
          modules = [
            hardwareConfig
            home-manager.nixosModules.home-manager
            { home-manager.extraSpecialArgs = { inherit inputs; }; }
            { nixpkgs.overlays = [ inputs.emacs-overlay.overlay ]; }
            sops-nix.nixosModules.sops
            ./nixos.nix
            ./system
            ./notarock
          ];
          pkgs = myPkgs;
        };

      mkBaseUser = { username, email, system }:
        let
          isDarwin = if system == "x86_64-linux" then false else true;
          rootUser = if isDarwin then "@admin" else "root";
          homePath =
            if isDarwin then "/Users/${username}" else "/home/${username}";
        in {
          home-manager.users.${username} = {
            home.username = username;
            programs.git.userEmail = email;
          };
          home-manager.extraSpecialArgs = { inherit inputs; };
          nix.settings.trusted-users = [ username rootUser ];
          users.users.${username} = {
            home = homePath;
            isHidden = false;
            shell = nixpkgs.zsh;
          };
        };

      mkDarwinConfiguration = { hostname, system, username, email }:
        darwin.lib.darwinSystem {
          system = system;
          specialArgs = { inherit inputs; };
          modules = [
            ./darwin.nix
            ./configuration.nix
            home-manager.darwinModules.home-manager
            (mkBaseUser {
              inherit username;
              inherit email;
              inherit system;
            })
            {
              home-manager.users.${username}.imports = [ ./notarock/roch.nix ];
            }
          ];
        };

    in {

      # NixOS configurations
      # nixos-rebuild switch -I nixos-config=hosts/Zonnarth/configuration.nix
      nixosConfigurations = {
        Zonnarth = mkNixosConfiguration { hostname = "Zonnarth"; };
        Kreizemm = mkNixosConfiguration { hostname = "Kreizemm"; username = "notarock"; };
      };

      # Darwin configurations
      darwinConfigurations = {
        Wistari = mkDarwinConfiguration { # Macbook Pro 16" 2019 for work
          hostname = "Wistari";
          username = "roch";
          email =
            "roch.damour@arctiq.ca"; # If you email me here I *will* ignore you.
          system = "x86_64-darwin";
        };

        Hectasio = mkDarwinConfiguration { # Mac M2 Max
          hostname = "Hectasio";
          username = "notarock";
          email =
            "roch.damour@gmail.com"; # If you email me here I *will* ignore you.
          system = "aarch64-darwin";
        };

        hectasio = mkDarwinConfiguration { # Mac M2 Max
          hostname = "Hectasio";
          username = "notarock";
          email =
            "roch.damour@gmail.com"; # If you email me here I *will* ignore you.
          system = "aarch64-darwin";
        };
      };

      # Home Manager configurations
      # For use with non NixOS/Darwin systems.
      homeConfigurations = {
        # NixOS desktop config
        rdamour = home-manager.lib.homeManagerConfiguration {
          extraSpecialArgs = { inherit inputs; };
          modules = [
            ./notarock/rdamour.nix
            {
              home = {
                username = "rdamour";
                homeDirectory = "/home/rdamour";
                stateVersion = "22.11";
              };
              nixpkgs.config = { allowUnfree = true; };
            }
          ];
        };
      };
      rdamour = self.homeConfigurations.bogdb.activationPackage;
    };
}
