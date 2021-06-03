{ config, pkgs, ... }:

{
  system.stateVersion = "21.11";
  nix.nixPath = [ "nixpkgs=${pkgs.path}" ];

  imports = [
    # <sop-nix/sops> TODO: Sopsify stuff
    ./core/enableFlake.nix
    ./core/system.nix
    ./core/keychron.nix
    ./core/services.nix
    ./core/systemPackages.nix
    ./notarock/notarock.nix
  ];

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
  };

  programs = {
    zsh.enable = true;
    ssh.startAgent = false;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "gtk2";
    };
  };

  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [ dejavu_fonts opensans-ttf font-awesome ];
  };

  environment.variables.EDITOR = "vim";
  programs.light.enable = true;

  # Fixes svg icon-theme
  # https://github.com/NixOS/nixpkgs/issues/13537#issuecomment-332327760
  # Use librsvg's gdk-pixbuf loader cache file as it enables gdk-pixbuf to load SVG files (important for icons)
  environment.sessionVariables = {
    GDK_PIXBUF_MODULE_FILE =
      "$(echo ${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/*/loaders.cache)";
  };

}
