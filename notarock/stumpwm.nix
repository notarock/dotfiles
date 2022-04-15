{ config, lib, pkgs, ... }:

{
  xdg.configFile."stumpwm" = {
    source = ./stumpwm;
    recursive = true;
  };
}
