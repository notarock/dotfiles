{ config, lib, pkgs, ... }:

with lib; {
  location.latitude = 45.5;
  location.longitude = -73.5;

  services = {
    xserver = {
      enable = true;
      displayManager.gdm.enable = true;
      desktopManager.gnome.enable = true;
      windowManager.herbstluftwm.enable = true;
      windowManager.xmonad.enable = true;
      windowManager.stumpwm.enable = false;
      windowManager.ratpoison.enable = false;
      windowManager.stumpwm-wrapper.enable = false;
      layout = "ca,fr";
      dpi = config.my.dpi;
    };

    redshift = {
      enable = true;
      temperature.day = 6500;
      temperature.night = 3000;
    };

    udev.packages = [ pkgs.yubikey-personalization pkgs.libu2f-host ];
    pcscd.enable = true;

    printing = {
      enable = true;
      drivers = with pkgs; [ hplip ];
    };

  };
  environment.sessionVariables.NIXOS_OZONE_WL = "1";
  environment.sessionVariables.OZONE_WL = "1";
}
