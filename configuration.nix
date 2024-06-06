{ config, pkgs, inputs, ... }:

{
  system.stateVersion = "24.11";
  nix.nixPath = [ "nixpkgs=${pkgs.path}" ];

  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  imports = [
    ./core/enableFlake.nix
    ./core/system.nix
    ./core/keychron.nix
    ./core/services.nix
    ./core/systemPackages.nix
    ./notarock
  ];

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
  sops.secrets.openai = {
    format = "binary";
    sopsFile = ./secrets/openai.txt;
    owner = "notarock";
  };

  programs = {
    zsh.enable = true;
    ssh.startAgent = false;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
  };

  fonts = {
    enableDefaultPackages = true;
    packages = with pkgs; [
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

  # Enable OpenGL
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
  };

  # Load nvidia driver for Xorg and Wayland
  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.nvidia = {

    # Modesetting is required.
    modesetting.enable = true;

    # Nvidia power management. Experimental, and can cause sleep/suspend to fail.
    # Enable this if you have graphical corruption issues or application crashes after waking
    # up from sleep. This fixes it by saving the entire VRAM memory to /tmp/ instead
    # of just the bare essentials.
    powerManagement.enable = false;

    # Fine-grained power management. Turns off GPU when not in use.
    # Experimental and only works on modern Nvidia GPUs (Turing or newer).
    powerManagement.finegrained = false;

    # Use the NVidia open source kernel module (not to be confused with the
    # independent third-party "nouveau" open source driver).
    # Support is limited to the Turing and later architectures. Full list of
    # supported GPUs is at:
    # https://github.com/NVIDIA/open-gpu-kernel-modules#compatible-gpus
    # Only available from driver 515.43.04+
    # Currently alpha-quality/buggy, so false is currently the recommended setting.
    open = false;

    # Enable the Nvidia settings menu,
    # accessible via `nvidia-settings`.
    nvidiaSettings = true;

    # Optionally, you may need to select the appropriate driver version for your specific GPU.
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

}
