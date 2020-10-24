# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page and in the NixOS manual (accessible by running ‘nixos-help’).
{ config, pkgs, ... }:

let
  my-theme = {
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
in {
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

  home-manager = {
    users.notarock = { pkgs, ... }: {
      home.keyboard.layout = "ca,fr";

      services.udiskie = {
        enable = true;
        automount = true;
        notify = true;
        tray = "auto";
      };

      services.polybar = {
        enable = true;
        config = {
          "settings" = {
            throttle-ms = 50;
            throttle-limit = 5;
          };

          "bar/main" = {
            font-0 = "Essential PragmataPro:size=11";
            # font-1 = "FontAwesome:size=8;-2"
            # font-2 = "ypn envypn:size=10;-1"
            # font-3 = "Termsynu:size=8;-1"
            # font-4 = "Unifont:size=6;-3"
            monitor = "\${env:MONITOR:DP-1}";
            width = "100%";
            height = "1.5%";
            radius = 0;
            bottom = true;
            background = my-theme.color0;
            foreground = my-theme.color7;
            overline-size = 0;
            overline-color = my-theme.color6;
            underline-size = 2;
            underline-color = my-theme.color6;
            spacing = 1;
            padding-right = 2;
            module-margin-left = 0;
            module-margin-right = 2;
            modules-left = "ewmh";
            modules-center = "my-hostname";
            modules-right = "volume cpu memory clock";
            tray-position = "right";
            tray-detached = false;
          };

          "module/my-hostname" = {
            type = "custom/text";
            content = "notarock @ ${config.networking.hostName}";
            format-foreground = my-theme.color7;
            format-underline = my-theme.color1;
          };

          "module/cpu" = {
            type = "internal/cpu";
            interval = 2;
            format = "<label> <ramp-coreload>";
            format-background = my-theme.color2;
            format-foreground = my-theme.color0;
            format-underline = my-theme.color2;
            format-overline = my-theme.color2;
            format-padding = 2;
            label = "cpu %percentage%%";
            label-font = 3;
            ramp-coreload-0 = "▁";
            ramp-coreload-0-font = 5;
            ramp-coreload-0-foreground = my-theme.color0;
            ramp-coreload-1 = "▂";
            ramp-coreload-1-font = 5;
            ramp-coreload-1-foreground = my-theme.color0;
            ramp-coreload-2 = "▃";
            ramp-coreload-2-font = 5;
            ramp-coreload-2-foreground = my-theme.color0;
            ramp-coreload-3 = "▄";
            ramp-coreload-3-font = 5;
            ramp-coreload-3-foreground = my-theme.color0;
            ramp-coreload-4 = "▅";
            ramp-coreload-4-font = 5;
            ramp-coreload-4-foreground = my-theme.color11;
            ramp-coreload-5 = "▆";
            ramp-coreload-5-font = 5;
            ramp-coreload-5-foreground = my-theme.color11;
            ramp-coreload-6 = "▇";
            ramp-coreload-6-font = 5;
            ramp-coreload-6-foreground = my-theme.color1;
            ramp-coreload-7 = "█";
            ramp-coreload-7-font = 5;
            ramp-coreload-7-foreground = my-theme.color1;
          };

          "module/memory" = {
            type = "internal/memory";
            format = "<label> <bar-used>";
            format-padding = 2;
            format-background = my-theme.color6;
            format-foreground = my-theme.color0;
            format-underline = my-theme.color6;
            format-overline = my-theme.color6;
            label = "memory %percentage_used%%";
            label-font = 3;
            bar-used-width = 10;
            bar-used-indicator = "|";
            bar-used-indicator-font = 4;
            bar-used-indicator-foreground = my-theme.color0;
            bar-used-fill = "─";
            bar-used-fill-font = 4;
            bar-used-fill-foreground = my-theme.color0;
            bar-used-empty = "─";
            bar-used-empty-font = 4;
            bar-used-empty-foreground = my-theme.color5;
          };

          "module/clock" = {
            type = "internal/date";
            date = "%%{T3}%Y-%m-%d %H:%M%%{T-}";
            format-padding = 2;
            format-background = my-theme.color1;
            format-foreground = my-theme.color0;
            format-underline = my-theme.color1;
            format-overline = my-theme.color1;
          };

          "module/volume" = {
            type = "internal/alsa";
            master-mixer = "Master";
            headphone-id = 9;
            format-volume-padding = 2;
            format-volume-background = my-theme.color3;
            format-volume-foreground = "#43433a";
            format-volume-underline = my-theme.color3;
            format-volume-overline = my-theme.color3;
            format-muted-padding = 2;
            format-muted-background = "#77ffff";
            format-muted-foreground = "#666666";
            label-volume = "volume %percentage%%";
            label-volume-font = 3;
            label-muted = "sound muted";
            label-muted-font = 3;
          };

          "module/ewmh" = {
            type = "internal/xworkspaces";
            enable-click = false;
            enable-scroll = false;
            label-active-foreground = "#ffffff";
            label-active-background = "#3f3f3f";
            label-active-padding = 4;
            label-empty-padding = 1;
          };
        };

        script = ''
        MONITOR=$(polybar -m | grep primary | awk '{print $1}' | sed '$s/.$//'); USER=$(whoami); polybar main &
        '';
      };

      xsession.windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        config = extras/xmonad/xmonad.hs ;
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

      programs.rofi = {
        enable = true;
        separator = "solid";
        font = "Essential PragmataPro 14";
        theme = "/etc/nixos/extras/rofi/conf";
        extraConfig = ''
      rofi.dpi: 0
    '';
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
        font_size = "13.0";
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
        theme = "agnoster";
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

      gtk = {
        enable = true;
        iconTheme.package = pkgs.numix-icon-theme-square;
        iconTheme.name = "Numix-Square";
        theme.package = pkgs.numix-gtk-theme ;
        theme.name = "Numix";
      };

    };
    useUserPackages = true;
    useGlobalPkgs = true;
  };

  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome3.enable = true;
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
  };

  services.xserver.layout = "ca,fr";
  services.xserver.dpi = 96;

  services.redshift = {
    enable = true;
    temperature.day = 6500;
    temperature.night = 3000;
  };

  location.latitude = 45.5;
  location.longitude = -73.5;

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

    nitrogen
    xorg.xmessage
    ghc
    slack

    gdk-pixbuf
    librsvg
    gnumake

    # pkglist
    ];



  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [
      dejavu_fonts
      opensans-ttf
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
  virtualisation.libvirtd.enable = true;
  virtualisation.docker.enable = true;

  services.udev.packages = [ pkgs.yubikey-personalization pkgs.libu2f-host ];
  services.pcscd.enable = true;
  # environment.shellInit = ''
  # export GPG_TTY="$(tty)"
  # gpg-connect-agent /bye
  # export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
  # '';

  # Fixes svg icon-theme
  # https://github.com/NixOS/nixpkgs/issues/13537#issuecomment-332327760
  # Use librsvg's gdk-pixbuf loader cache file as it enables gdk-pixbuf to load SVG files (important for icons)
  environment.sessionVariables = {
    GDK_PIXBUF_MODULE_FILE = "$(echo ${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/*/loaders.cache)";
  };

}
