{ confi, lib, pkgs, inputs, osConfig, ... }:

{
  imports = [
    # Linux specifics
    ./dunst.nix
    ./polybar.nix

    # Cross platform
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

    eza = {
      enable = true;
    };

    feh.enable = true;

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
      enable = true;
      # separator = "solid";
      font = "Essential PragmataPro 14";
      # theme = "/etc/nixos/extras/rofi/conf";
      plugins = with pkgs; [ rofi-emoji ];
      extraConfig = { dpi = osConfig.my.dpi; };
    };

    git = {
      difftastic = {
        enable = true;
        background = "light";
        color = "always";
      };
      delta.enable = false;
      enable = true;
      userName = "Roch D'Amour";
      extraConfig = { pull.rebase = false; };
    };
  };

}
