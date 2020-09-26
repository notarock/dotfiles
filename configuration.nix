# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page and in the NixOS manual (accessible by running ‘nixos-help’).
{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
  # nixpkgs.config.permittedInsecurePackages = [
  #   "openssl-1.0.2u"
  # ];

  imports = [ # Include the results of the hardware scan.
    ./host/hardware-configuration.nix # Host-specific hardware configuration
    <home-manager/nixos>
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
    shell = pkgs.zsh;
  };

  home-manager = {
    users.roche = { pkgs, ... }: {
      programs.zsh = {
        enable = true;
        shellAliases = {
          ll = "ls -alF";
          la = "ls -A";
          l = "ls -CF";
          cp = "cp -i";
          df = "df -h";
          cdsrc = "cd ~/src/";
          ".." = "cd ..";
          "..." = "cd ../..";
          "...." = "cd ../../..";
          "....." = "cd ../../../..";
          vi = "vim";
          vif = "vim \$\(fzf\)";
          lla = "ls -la";
          lt = "ls -lrt";
          dstop = "docker stop $(docker ps -a -q)";
          dclean = "docker rm $(docker ps -a -q)";
          dclear = "docker rmi $(docker images -q)";
          open = "$FILEMANAGER";
          nixc = "sudo $EDITOR /etc/nixos/configuration.nix";
          nbs = "sudo nixos-rebuild switch";
          wttr = "curl wttr.in";
          k = "kubectl";
        };
      };

      programs.kitty = {
        enable = true;
        font.package = pkgs.iosevka;
        font.name = "Iosevka Regular 20";
        settings = {
          enable_audio_bell = false;
          open_url_with = "firefox";
          scrollback_lines = 5000;
          cursor_shape = "block";
          cursor_blink_interval = "1.0";
          cursor_stop_blinking_after = "1.0";
          cursor_text_color = "background";
          copy_on_select = "no";
          mouse_hide_wait = "3.0";
          sync_to_monitor = "yes";
          enabled_layouts =  "Vertical";

          # Base16 Helios - kitty color config
          # Scheme by Alex Meyer (https://github.com/reyemxela)
          background = "#1d2021";
          foreground = "#d5d5d5";
          selection_background = "#d5d5d5";
          selection_foreground = "#1d2021";
          url_color = "#cdcdcd";
          cursor = "#d5d5d5";
          active_border_color = "#6f7579";
          inactive_border_color = "#383c3e";
          active_tab_background = "#1d2021";
          active_tab_foreground = "#d5d5d5";
          inactive_tab_background = "#383c3e";
          inactive_tab_foreground = "#cdcdcd";
          tab_bar_background = "#383c3e";

          color0 = "#1d2021";
          color1 = "#d72638";
          color2 = "#88b92d";
          color3 = "#f19d1a";
          color4 = "#1e8bac";
          color5 = "#be4264";
          color6 = "#1ba595";
          color7 = "#d5d5d5";
          color8 = "#6f7579";
          color9 = "#eb8413";
          color10 = "#383c3e";
          color11 = "#53585b";
          color12 = "#cdcdcd";
          color13 = "#dddddd";
          color14 = "#c85e0d";
          color15 = "#e5e5e5";

        };

      };

      programs.zsh.oh-my-zsh = {
        enable = true;
        theme = "mh";
        plugins = [
          "git"
          "git-flow"
          "git-extras"
          "pass"
          "docker"
          "kubectl"
          "vagrant"
          "npm"
          "node"
          "python"
        ];
        extraConfig = "
          export PATH=$HOME/bin:/usr/local/bin:$PATH
          export PATH=$HOME/snap:$PATH
          export PATH=$HOME/.emacs.d/bin/:$PATH
        ";

      };

      gtk.iconTheme.package = pkgs.numix-icon-theme-square;
      gtk.iconTheme.name = "Numix-Square";
      gtk.theme.package = pkgs.numix-gtk-theme ;
      gtk.theme.name = "Numix";

    };
    useUserPackages = true;
    useGlobalPkgs = true;
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
    gnome3.gnome-tweaks
    bibata-cursors
    capitaine-cursors
    riot-desktop
    nextcloud-client

    pkgs.numix-icon-theme-square
    pkgs.numix-gtk-theme

    woeusb
    jdk11
    jre
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

    (sudo.override {
      withInsults = true;
    })

    # Gnome ext
    gnomeExtensions.dash-to-dock
    gnomeExtensions.caffeine
    gnomeExtensions.system-monitor
    gnomeExtensions.impatience

    spotify
    ccls
    clang-tools

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
