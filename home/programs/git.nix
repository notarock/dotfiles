{ config, lib, pkgs, ... }:

{
  programs = {
    git = {
      enable = true;
      settings.user.name = "Roch D'Amour";
      extraConfig = { pull.rebase = false; };
    };
    difftastic = {
      enable = true;
      background = "light";
      color = "always";
    };
    delta.enable = false;
    diff-so-fancy.enable = true;
  };
}
