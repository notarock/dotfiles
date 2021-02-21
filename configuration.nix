{ config, pkgs, ... }:

let
  hostSpecific = import ./host/variables.nix;
  my-theme = import ./users/notarock/theme.nix;
in {
  system.stateVersion = "20.09";

  imports = [ # Include the results of the hardware scan.
    # Contains host-specific configuration.
    # In order for this import to work, you have to link the desired
    # host folder to `host` in this repo's root. Like this:
    # ln -s hosts/zonnarth host
    <home-manager/nixos>
    ./core/system.nix
    ./core/services.nix
    ./core/systemPackages.nix
    ./host/hardware-configuration.nix
    ./users/notarock/user.nix
  ];

  programs = {
    zsh.enable = true;
    ssh.startAgent = false;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "gtk2";
    };
  };

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
  };

  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [ dejavu_fonts opensans-ttf font-awesome ];
  };

  environment.variables.EDITOR = "vim";

  # Fixes svg icon-theme
  # https://github.com/NixOS/nixpkgs/issues/13537#issuecomment-332327760
  # Use librsvg's gdk-pixbuf loader cache file as it enables gdk-pixbuf to load SVG files (important for icons)
  environment.sessionVariables = {
    GDK_PIXBUF_MODULE_FILE =
      "$(echo ${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/*/loaders.cache)";
  };

  # Turns out it's quite annoying
  #
  # services.cron = {
  #   enable = true;
  #   systemCronJobs = let
  #     dodocron =
  #       "notarock  XDG_RUNTIME_DIR=/run/user/$(id -u) notify-send BEDTIME 'Time to go to sleep'";
  #   in [
  #     "*/1 0-5 * * 0-5  ${dodocron}"
  #     "30/1 23 * * 0-5  ${dodocron}"
  #     "* 3 * * 5-6      ${dodocron}"
  #   ];
  # };

}
