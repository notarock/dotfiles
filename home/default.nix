{ config, lib, pkgs, inputs, ... }:

let inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;
in {
  imports = [ ./programs ./options ./myTheme.nix ./packages.nix ./xdg.nix ];

  targets.genericLinux = (lib.mkIf (isLinux) { enable = true; });
  nixpkgs.config = { allowUnfree = true; };

  programs.bash.enable = true;

  services.nextcloud-client.enable = isLinux;

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

    # rofi = {
    #   enable = true;
    #   # separator = "solid";
    #   font = "Essential PragmataPro 14";
    #   # theme = "/etc/nixos/extras/rofi/conf";
    #   plugins = with pkgs; [ rofi-emoji ];
    # };

    git = {
      enable = true;
      settings.user.name = "Roch D'Amour";
    };
    delta.enable = false;
    diff-so-fancy.enable = true;
    difftastic.enable = false;
  };

}
