{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ nodePackages.pyright yaml-language-server ];

  # services.emacs = {
  #   enable = true;
  #   package = config.programs.emacs.package;
  # };

  programs.doom-emacs = {
    enable = true;
    emacsPackage = pkgs.emacsGit;
    doomPrivateDir = ../doom.d;
  };
}
