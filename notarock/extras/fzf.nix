{ config, lib, pkgs, ... }:

let my-theme = import ../../themes/base16-ia-dark.nix;
in {
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
    defaultCommand =
      ''${pkgs.fd}/bin/fd --follow --type f --exclude="'.git'" .'';
    defaultOptions = [ "--exact" "--cycle" "--layout=reverse" ];
    enableFishIntegration = false;
  };
}
