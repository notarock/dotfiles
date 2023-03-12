{ config, pkgs, inputs, ... }:

{
  system.stateVersion = "22.11";
  nix.nixPath = [ "nixpkgs=${pkgs.path}" ];

  imports = [
    ./core/enableFlake.nix
    ./core/system.nix
    ./core/keychron.nix
    ./core/services.nix
    ./core/systemPackages.nix
    ./notarock
  ];

  services.xserver.displayManager.defaultSession = "plasmawayland";
  services.xserver.desktopManager.wallpaper.mode = "tile";

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
  };

  services.sshd.enable = true;

  # Sops config
  sops.defaultSopsFile = ./secrets/notarock.yaml;
  sops.secrets.password = { };
  sops.secrets.pragmatapro-reg = {
    format = "binary";
    sopsFile = ./secrets/EssentialPragmataPro-R.ttf;
    owner = "notarock";
  };
  sops.secrets.pragmatapro-reg-otf = {
    format = "binary";
    sopsFile = ./secrets/EssentialPragmataPro-R.otf;
    owner = "notarock";
  };
  sops.secrets.pragmatapro-bold = {
    format = "binary";
    sopsFile = ./secrets/EssentialPragmataPro-B.ttf;
    owner = "notarock";
  };
  sops.secrets.pragmatapro-bold-otf = {
    format = "binary";
    sopsFile = ./secrets/EssentialPragmataPro-B.otf;
    owner = "notarock";
  };
  sops.secrets.wakatime = {
    format = "binary";
    sopsFile = ./secrets/wakatime.cfg;
    owner = "notarock";
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
    fonts = with pkgs; [
      nerdfonts
      dejavu_fonts
      open-sans
      font-awesome
      ibm-plex
    ];
  };

  environment.variables.EDITOR = "vim";
  programs.light.enable = true;

  # Fixes svg icon-theme
  # https://github.com/NixOS/nixpkgs/issues/13537#issuecomment-332327760
  # Use librsvg's gdk-pixbuf loader cache file as it enables gdk-pixbuf to load SVG files (important for icons)
  # environment.sessionVariables = {
  #   GDK_PIXBUF_MODULE_FILE =
  #     "$(echo ${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/*/loaders.cache)";
  # };

  # Minimal configuration for NFS support with Vagrant.
  services.nfs.server.enable = true;
  networking.firewall.extraCommands = ''
    ip46tables -I INPUT 1 -i vboxnet+ -p tcp -m tcp --dport 2049 -j ACCEPT
  '';

  programs.noisetorch.enable = true;

  security.sudo.extraRules = [
    {
      users = [ "notarock" ];
      commands = [{
        command = "/run/current-system/sw/bin/nixos-rebuild";
        options = [ "NOPASSWD" ];
      }];
    }
    {
      users = [ "ALL" ];
      commands = [
        {
          command = "${pkgs.docker}/bin/docker ps -qf status=dead";
          options = [ "NOPASSWD" ];
        }
        {
          command = "${pkgs.docker}/bin/docker ps -qf status=running";
          options = [ "NOPASSWD" ];
        }
        {
          command = "${pkgs.docker}/bin/docker ps -qf status=restarting";
          options = [ "NOPASSWD" ];
        }
      ];
    }
  ];
  systemd.services.NetworkManager-wait-online.enable = false;

}
