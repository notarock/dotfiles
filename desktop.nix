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

    };
  };

  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [
      dejavu_fonts
      iosevka
      ibm-plex
      hack-font
      # input-fonts - Must be downloaded from their website at
    ];
  };

}
