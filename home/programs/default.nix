{ confi, lib, pkgs, inputs, osConfig, ... }:

let inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;
in {
  imports = [
    # Linux specifics
    ./dunst.nix
    ./herbstluftwm.nix
    ./polybar.nix
    ./xmonad.nix
    ./udiskie.nix
    ./gui.nix

    # Cross platform
    ./emacs.nix
    ./go.nix
    ./kitty.nix
    ./vim.nix
    ./fzf.nix
    ./starship.nix
    ./zsh.nix
  ];

  programs = {
    command-not-found.enable = true;

    direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv = { enable = true; };
    };

    eza = { enable = true; };

    feh.enable = isLinux;

    gh = {
      enable = true;
      settings = {
        git_protocol = "ssh";

        prompt = "enabled";
      };
    };

    broot = {
      enable = true;
      enableZshIntegration = true;
    };

    rofi = {
      enable = isLinux;
      # separator = "solid";
      font = "Essential PragmataPro 14";
      # theme = "/etc/nixos/extras/rofi/conf";
      plugins = with pkgs; [ rofi-emoji ];
      extraConfig = { dpi = osConfig.my.dpi; };
    };

  };

}
