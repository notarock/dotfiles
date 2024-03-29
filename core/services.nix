{ config, lib, pkgs, ... }:

with lib; {
  location.latitude = 45.5;
  location.longitude = -73.5;

  programs.dconf.enable = true;

  services = {
    xserver = {
      enable = true;
      displayManager.sddm.enable = false;
      desktopManager.plasma5.enable = false;
      desktopManager.plasma5.excludePackages = with pkgs.libsForQt5; [
        elisa
        gwenview
        okular
        oxygen
        khelpcenter
        konsole
        plasma-browser-integration
        print-manager
      ];

      displayManager.gdm.enable = true;
      desktopManager.gnome.enable = true;
      windowManager.herbstluftwm.enable = true;

      windowManager.xmonad.enable = false;
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
}
