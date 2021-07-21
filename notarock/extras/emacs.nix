{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    python-language-server
    yaml-language-server
  ];

  # services.emacs = {
  #   enable = true;
  #   package = config.programs.emacs.package;
  # };

  programs.doom-emacs = {
    enable = true;
    doomPrivateDir = ../doom.d;
  };
}
