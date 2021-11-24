{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    nodePackages.pyright
    yaml-language-server
    gopls
  ];

  programs.doom-emacs = {
    enable = true;
    emacsPackage = pkgs.emacsGit;
    doomPrivateDir = ../doom.d;
  };

  home.file.".emacs.d/init.el".text = ''
    (setq wakatime-cli-path "${pkgs.wakatime}/bin/wakatime-cli")
  '';
}
