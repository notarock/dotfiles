{ config, lib, pkgs, ... }:

let
  my-theme = import ./theme.nix;
in {
  users.users.notarock = {
    isNormalUser = true;
    home = "/home/notarock";
    description = "Notarock";
    extraGroups = [ "wheel" "docker" ];
    shell = pkgs.zsh;
    initialPassword = "Ch4ngeMoi%%%";
  };

  home-manager = {
    users.notarock = { pkgs, ... }: {
      gtk = {
        enable = true;
        iconTheme.package = pkgs.numix-icon-theme-square;
        iconTheme.name = "Numix-Square";
        theme.package = pkgs.amber-theme;
        theme.name = "Amber";
      };


      imports = [
        ./home-manager/herbstluftwm.nix
        ./home-manager/polybar.nix
        ./home-manager/udiskie.nix
        ./home-manager/dunst.nix
        ./home-manager/vim.nix
        ./home-manager/zsh.nix
        ./home-manager/fzf.nix
        ./home-manager/kitty.nix
      ];

      programs.emacs = {
        enable = true;
        package = pkgs.emacs27;
      };

      programs.rofi = {
        enable = true;
        separator = "solid";
        font = "Essential PragmataPro 14";
        theme = "/etc/nixos/extras/rofi/conf";
        extraConfig = ''
          rofi.dpi: 0
        '';
      };

      programs.git = {
        delta.enable = true;
        enable = true;
        userName = "Roch D'Amour";
        userEmail = "roch.damour@gmail.com";
        extraConfig = { pull.rebase = false; };
      };

      xresources.properties = {
        "xft.dpi" = "144";
        "XTerm*faceName" = "dejavu sans mono";
        "Xcursor.size" = "32";
        "Xcursor.theme" = "Bibata Oil";
      };

      home.keyboard.layout = "ca,fr";
      services.nextcloud-client.enable = true;

    };
  };

}
