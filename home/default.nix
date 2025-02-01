{ config, pkgs, inputs, ... }:

{
  imports = [
    ./programs
    ./options

    ./myTheme.nix
    ./packages.nix
    ./xdg.nix
  ];

  targets.genericLinux.enable = true;

  programs.bash.enable = true;
  services.nextcloud-client.enable = true;

  home.keyboard.layout = "ca,fr";

  myTheme = import ../themes/base16-brewer.nix;

  manual = {
    html.enable = true;
    manpages.enable = true;
  };

  programs = {
    # feh.enable = true;

    gh = {
      enable = true;
      settings = {
        git_protocol = "ssh";

        prompt = "enabled";
      };
    };

    # broot = {
    #   enable = true;
    #   enableZshIntegration = true;
    # };

    rofi = {
      enable = true;
      # separator = "solid";
      font = "Essential PragmataPro 14";
      # theme = "/etc/nixos/extras/rofi/conf";
      plugins = with pkgs; [ rofi-emoji ];
    };

    git = {
      delta.enable = false;
      diff-so-fancy.enable = true;
      difftastic.enable = false;
      enable = true;
      userName = "Roch D'Amour";
    };
  };
}
