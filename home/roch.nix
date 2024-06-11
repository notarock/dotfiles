{ config, pkgs, inputs, ... }:

{
  imports = [
    ./programs.nix
    ./myTheme.nix
    ./packages.nix
    ./extras/vim.nix
    ./extras/emacs.nix
    ./extras/zsh.nix
    ./extras/fzf.nix
    ./extras/kitty.nix
    ./extras/starship.nix
  ];

  myTheme = import ../themes/base16-monokai.nix;

  manual = {
    html.enable = true;
    manpages.enable = true;
  };

  home = {
    stateVersion = "22.11";
    enableNixpkgsReleaseCheck = true;
  };

  home.keyboard.layout = "ca,fr";

  xdg.configFile."scripts/emacs.sh" = {
    executable = true;
    text = ''
      #!/bin/zsh

      # Required parameters:
      # @raycast.schemaVersion 1
      # @raycast.title emacs
      # @raycast.mode silent

      # Optional parameters:
      # @raycast.icon 🤖

      # Documentation:
      # @raycast.description emacs from shell
      # @raycast.author Roch

      # zsh -c /Users/roch/.nix-profile/bin/emacs

      osascript -e 'activate application "emacs"'
    '';
  };

}
