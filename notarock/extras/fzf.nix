
{ config, lib, pkgs, ... }:

let
  my-theme = import ../../themes/base16-solarflare.nix;
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
