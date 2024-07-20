{ config, lib, pkgs, ... }:

{
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
}
