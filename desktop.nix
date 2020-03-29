{ pkgs, ... }:

{
  services = {
    # Print logs to tty12
    journald.console = "/dev/tty12";
    printing.enable = true;

    # Enable the x11 windowing system
    xserver = {
      # Display Manager (User selection and stuff)
      displayManager.gdm.enable = true;
      displayManager.gdm.autoLogin.enable = false;

      # Gnome
      displayManager.gdm.wayland = false;
      desktopManager.gnome3.enable = true;

      # KDE
      desktopManager.plasma5.enable = true;

      enable = true;
      layout = "ca,fr";
      dpi = 144;

      # Enable touchpad support.
      libinput.enable = true;
      wacom.enable = true;

      windowManager.i3 = {
        enable = true;
        extraPackages = with pkgs; [
          dmenu # application launcher most people use
          i3status # gives you the default i3 status bar
          i3lock # default i3 screen locker
          i3blocks # if you are planning on using i3blocks over i3status
        ];
      };

    };
  };

  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [
      dejavu_fonts
      iosevka
      ibm-plex
      hack-font
      monoid
      ankacoder
      ankacoder-condensed
      # input-fonts - Must be downloaded from their website at
    ];
  };

}
