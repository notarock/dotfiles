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
      displayManager.gdm.wayland = true;
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
}
