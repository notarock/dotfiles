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


  networking.hostName = "Kreizemm"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;
  hardware.bluetooth.enable = true;

  networking.extraHosts = ''
    192.168.10.10 homestead.test
  '';

  #  65.39.166.132 = Cogeco Montréal
  #  1.1.1.1       = Cloudflare
  #  9.9.9.9       = Quad-9
  #
  #
  networking.nameservers = ["65.39.166.132"  "1.1.1.1" "9.9.9.9"];

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
    ssh.startAgent = false;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      # pinentryFlavor = "curses";
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
  system.stateVersion = "20.09"; # Did you read the comment? YES

  system.autoUpgrade.enable = false;
  system.autoUpgrade.allowReboot = true;

  # Fix Intel CPU throttling effecting ThinkPads
  services = {
    emacs.enable = false;
    # AMD cpu time
    # throttled.enable = true;
  };

  boot.plymouth.enable = false;

  boot.supportedFilesystems = [ "ntfs" ];

  users.users.notarock = {
    isNormalUser = true;
    home = "/home/notarock";
    description = "notarock";
    extraGroups = [ "wheel" "docker"];
    shell = pkgs.zsh;
  };

  # nixpkgs.overlays = [
  #   (final: super: {
  #     lispPackages.stumpwm = super.lispPackages.stumpwm.overrideAttrs (
  #       oldAttrs: rec {
  #         propagatedBuildInputs = with super; [
  #           lispPackages.clx-truetype
  #           lispPackages.xembed
  #           lispPackages.swank
  #           lispPackages.quicklisp
  #         ] ++ (oldAttrs.propagatedBuildInputs or []);
  #       }
  #     );
  #   })
  # ];

  home-manager = {
    users.notarock = { pkgs, ... }: {
      home.keyboard.layout = "ca,fr";
      # xsession = {
      #   enable = true;
      #   initExtra = ''
      #       export WM=stumpwm
      #     '';
      #   windowManager.command = ''
      #       ${pkgs.lispPackages.stumpwm}/bin/stumpwm-lisp-launcher.sh \
      #         --eval '(require :clx-truetype)' \
      #         --eval '(require :xembed)' \
      #         --eval '(require :swank)' \
      #         --eval '(require :asdf)' \
      #         --eval '(asdf:load-system :stumpwm)' \
      #         --eval '(stumpwm:stumpwm)'
      #     '';
      # };

      xsession.windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };

      xresources.properties = {
        "XTerm*faceName" = "dejavu sans mono";
        "Xcursor.size"= "32";
        "Xcursor.theme"= "Bibata Oil";
      };

      programs.git = {
        enable = true;
        userName  = "Roch D'Amour";
        userEmail = "roch.damour@gmail.com";
      };

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
          randpw = "dd if=/dev/urandom bs=1 count=64 2>/dev/null | base64 -w 0 | rev | cut -b 2- | rev";
        };
      };

      programs.kitty = {
        enable = true;
        font.name = "Essential PragmataPro";
        settings = {
          font_size = "12.0";
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

          # Base16 Darktooth - kitty color config
          # Scheme by Jason Milkins (https://github.com/jasonm23)
          background = "#1d2021";
          foreground = "#a89984";
          selection_background = "#a89984";
          selection_foreground = "#1d2021";
          url_color = "#928374";
          cursor = "#a89984";
          active_border_color = "#665c54";
          inactive_border_color = "#32302f";
          active_tab_background = "#1d2021";
          active_tab_foreground = "#a89984";
          inactive_tab_background = "#32302f";
          inactive_tab_foreground = "#928374";
          tab_bar_background = "#32302f";

          # normal
          color0 = "#1d2021";
          color1 = "#fb543f";
          color2 = "#95c085";
          color3 = "#fac03b";
          color4 = "#0d6678";
          color5 = "#8f4673";
          color6 = "#8ba59b";
          color7 = "#a89984";

          # bright
          color8 =  "#665c54";
          color9 =  "#fe8625";
          color10 = "#32302f";
          color11 = "#504945";
          color12 = "#928374";
          color13 = "#d5c4a1";
          color14 = "#a87322";
          color15 = "#fdf4c1";
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
  services.xserver.dpi = 96;

  # services.xserver = {
  #   windowManager.xmonad = {
  #     enable = true;
  #     enableContribAndExtras = true;
  #     extraPackages = haskellPackages: [
  #       haskellPackages.xmonad-contrib
  #       haskellPackages.xmonad-extras
  #       haskellPackages.xmonad
  #     ];
  #   };
  #   windowManager.default = "xmonad";

  #   displayManager.sessionCommands = with pkgs; lib.mkAfter
  #     ''
  #     xmodmap /path/to/.Xmodmap
  #     '';
  # };


  environment.systemPackages = with pkgs; [
    qemu_kvm

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
    element-desktop
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

    tldr

    spotify
    ccls
    clang-tools
    wakatime

    texlive.combined.scheme-medium

    go
    gocode
    gopls

    pandoc

    zoom-us # School stuff
    yubioath-desktop

    virt-manager
    gnome3.dconf

    ];



  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [
      dejavu_fonts
    ];
  };

  # G-word (g*ming)
  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
  hardware.pulseaudio.support32Bit = true;

  # Keychron k8 fn keys stuff
  boot.extraModprobeConfig = ''
    options hid_apple fnmode=2
  '';
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelModules = [ "kvm-amd" ];
  virtualisation.libvirtd.enable = true;
  virtualisation.docker.enable = true;

  services.udev.packages = [ pkgs.yubikey-personalization pkgs.libu2f-host  ];
  services.pcscd.enable = true;

  # environment.shellInit = ''
  # export GPG_TTY="$(tty)"
  # gpg-connect-agent /bye
  # export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
  # '';


}
