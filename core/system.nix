{ config, lib, pkgs, ... }:

let
  # unstableTarball = fetchTarball
  #   "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz";
  # masterTarball =
  #   fetchTarball "https://github.com/NixOS/nixpkgs/archive/master.tar.gz";
in {
  nixpkgs.config = {
    allowUnfree = true;
    allowBroken = false;
    # packageOverrides = pkgs: {
    #   unstable = import unstableTarball { config = config.nixpkgs.config; };
    #   master = import masterTarball { config = config.nixpkgs.config; };
    # };
  };

  imports = [ ./my/dpi.nix ./my/emacs.fontSize.nix ./my/emacs.fontSizeBig.nix ];

  nix.settings.auto-optimise-store = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.cleanTmpDir = true;

  networking.networkmanager.enable = true;
  hardware.bluetooth.enable = true;

  networking.nameservers = [
    "65.39.166.132" # Cogeco Montréal
    "1.1.1.1" # Cloudflare
    "9.9.9.9" # Quad-9
  ];

  i18n.defaultLocale = "en_CA.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Set your time zone.
  time.timeZone = "America/Montreal";

  sound.enable = true;

  system.autoUpgrade.enable = false;
  system.autoUpgrade.allowReboot = true;

  boot = {
    plymouth.enable = false;
    supportedFilesystems = [ "ntfs" ];
    # Keychron k8 fn keys stuff
    extraModprobeConfig = ''
      options hid_apple fnmode=0
    '';
  };

  boot.kernelPackages = pkgs.linuxPackages_latest;

  virtualisation = {
    libvirtd.enable = false;
    virtualbox.host.enable = true;
    docker.enable = true;
  };

  users.extraGroups.vboxusers.members = [ "notarock" ];

  # G-word (g*ming) stuff
  hardware = {
    opengl = {
      driSupport32Bit = true;
      extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
    };
    pulseaudio.enable = true;
    pulseaudio.support32Bit = true;
  };

}
