{ config, lib, pkgs, ... }:

let
  gapWidth = "10";
  tags = [ "1" "2" "3" "4" "5" "6" "7" "8" "9" ];
in {

  xdg.configFile."xmonad" = {
    source = ../xmonad;
    recursive = true;
  };

  services.taffybar.enable = true;

  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    # config = lib.mkDefault /home/.xmonad/xmonad.hs
  };
}
