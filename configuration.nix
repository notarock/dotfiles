# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
  # nixpkgs.config.permittedInsecurePackages = [
  #   "openssl-1.0.2u"
  # ];

  imports = [ # Include the results of the hardware scan.
    ./host/hardware-configuration.nix # Host-specific hardware configuration
    # ./system-packages.nix # Contains all system packages required
    # ./notarock.nix # Nickname for root
    # ./gaming.nix # Stuff that makes steam work
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.cleanTmpDir = true;

  # Virtualization
  virtualisation.docker.enable = true;
  virtualisation.virtualbox.host.enable = true;
  virtualisation.virtualbox.guest.enable = false;

  networking.hostName = "Labrue-nix"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;
  hardware.bluetooth.enable = true;

  networking.extraHosts = ''
    192.168.10.10 homestead.test
  '';

  networking.nameservers = ["1.1.1.1" "9.9.9.9"];

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_CA.UTF-8";

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Set your time zone.
  time.timeZone = "America/Montreal";

  programs = {
    # CLI stuff
    zsh.enable = true;
    zsh.ohMyZsh.enable = true;

    # Encryption key stuff
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
  };

  # List packages installed in system profile. To search, run:
  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment? YES

  system.autoUpgrade.enable = false;
  system.autoUpgrade.allowReboot = true;

  # Fix Intel CPU throttling effecting ThinkPads

  services = {
    emacs.enable = false;
    throttled.enable = true;
  };

  boot.plymouth.enable = false;

  boot.supportedFilesystems = [ "ntfs" ];

  users.users.roche = {
    isNormalUser = true;
    home = "/home/roche";
    description = "roche";
    extraGroups = [ "wheel" ];
  };

  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome3.enable = true;

      services.xserver.layout = "ca,fr";
      services.xserver.dpi = 144;

  environment.systemPackages = with pkgs; [

    wget curl
    vim neovim emacs
    git tig
    ack tree exa fd ripgrep bc bat
    gnupg pass
    zip unzip
    colordiff
    rsync
    htop gotop
    lsof pstree
    nmap parted ranger stow
    traceroute telnet tcpdump whois dnsutils mtr
    docker-compose vagrant xorg.xhost

      #
      # Programs
      #
      kitty
      libreoffice
      evince
      gnome3.cheese
      gnome3.evolution
      gnome3.gedit
      gnome3.pomodoro
      gimp
      krita
      vlc
      obs-studio
      arandr
      peek
      transmission
      #jetbrains.datagrip
      #jetbrains.idea-community
      postman
      #
      # Web Browsers
      #
      chromium
      firefox
      #
      # muh games
      #
      brogue
      discord
      steam
      minecraft
      #
      # Terminal utility
      #
      scrot
      rofi
      rofi-pass
      feh
      rhythmbox
      screenkey
      scrot
      neofetch # This needs to be included with every distro.

      unrar

      #
      # Eye candy
      #
      moka-icon-theme
      papirus-icon-theme
      paper-icon-theme
      paper-gtk-theme
      numix-solarized-gtk-theme
      numix-gtk-theme
      arc-theme
      gnome3.gnome-tweaks
      bibata-cursors
      capitaine-cursors
      riot-desktop

      #
      # Devops tooling
      #
      kubectl
      minikube
      kompose
      k9s
      terraform
      kubernetes-helm
      google-cloud-sdk
      ansible
      (steam.override {
        extraPkgs = pkgs: [ mono gtk3 gtk3-x11 libgdiplus zlib ];
        nativeOnly = true;
      }).run
    ];


  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [
      dejavu_fonts
      iosevka
      hack-font
    ];
  };


  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
  hardware.pulseaudio.support32Bit = true;

}
