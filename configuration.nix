{ config, pkgs, ... }:

let
  hostSpecific = import ./host/variables.nix;
  unstableTarball = fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz";
  masterTarball =
    fetchTarball "https://github.com/NixOS/nixpkgs/archive/master.tar.gz";
  my-theme = {
    color0 = "#0c0d0e";
    color1 = "#e31a1c";
    color2 = "#31a354";
    color3 = "#dca060";
    color4 = "#3182bd";
    color5 = "#756bb1";
    color6 = "#80b1d3";
    color7 = "#b7b8b9";
    color8 = "#737475";
    color9 = "#e6550d";
    color10 = "#2e2f30";
    color11 = "#515253";
    color12 = "#959697";
    color13 = "#dadbdc";
    color14 = "#b15928";
    color15 = "#fcfdfe";
  };
in {
  system.stateVersion = "20.09";

  nixpkgs.config = {
    allowUnfree = true;
    allowBroken = false;
    packageOverrides = pkgs: {
      unstable = import unstableTarball { config = config.nixpkgs.config; };
      master = import masterTarball { config = config.nixpkgs.config; };
    };
  };
  imports = [ # Include the results of the hardware scan.
    # Contains host-specific configuration.
    # In order for this import to work, you have to link the desired
    # host folder to `host` in this repo's root. Like this:
    # ln -s hosts/zonnarth host
    ./host/hardware-configuration.nix
    <home-manager/nixos>
  ];

  # Use the systemd-boot EFI boot loader.
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

  programs = {
    zsh.enable = true;

    ssh.startAgent = false;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "gtk2";
    };
  };

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  system.autoUpgrade.enable = false;
  system.autoUpgrade.allowReboot = true;

  boot.plymouth.enable = false;
  boot.supportedFilesystems = [ "ntfs" ];

  users.users.notarock = {
    isNormalUser = true;
    home = "/home/notarock";
    description = "Notarock";
    extraGroups = [ "wheel" "docker" ];
    shell = pkgs.zsh;
    initialPassword = "Ch4ngeMoi%%%";
  };

  home-manager = {
    users.notarock = { pkgs, ... }: {

      home.keyboard.layout = "ca,fr";

      services.nextcloud-client.enable = true;

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
            font-0 = "Essential PragmataPro:size=14";
            monitor = hostSpecific.mainMonitor;
            width = "100%";
            height = 30;
            radius = 0;
            top = true;
            # bottom = true;
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
            format-background = my-theme.color4;
          };

          "module/cpu" = {
            type = "internal/cpu";
            interval = 2;
            format = "<label>";
            format-background = my-theme.color2;
            format-foreground = my-theme.color0;
            format-underline = my-theme.color2;
            format-overline = my-theme.color2;
            format-padding = 2;
            label = "CPU %percentage%%";
          };

          "module/memory" = {
            type = "internal/memory";
            format = "<label>";
            format-padding = 2;
            format-background = my-theme.color6;
            format-foreground = my-theme.color0;
            format-underline = my-theme.color6;
            format-overline = my-theme.color6;
            label = "RAM %percentage_used%%";
            label-font = 3;
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
          sleep 3 && USER=$(whoami); polybar main &
        '';
      };

      services.dunst = {
        enable = true;
        settings = {
          global = {
            font = "Essential PragmataPro 12";
            markup = "full";
            format = "<b>%s</b>\\n%b";
            sort = "no";
            indicate_hidden = "yes";
            alignment = "left";
            bounce_freq = 0;
            show_age_threshold = -1;
            word_wrap = "yes";
            ignore_newline = "no";
            stack_duplicates = "yes";
            hide_duplicate_count = "yes";
            geometry = "600x100-1620+50";
            shrink = "no";
            transparency = 3;
            idle_threshold = 0;
            monitor = 0;
            follow = "none";
            sticky_history = "yes";
            history_length = 15;
            show_indicators = "no";
            line_height = 3;
            separator_height = 2;
            padding = 6;
            horizontal_padding = 6;
            separator_color = "frame";
            startup_notification = "false";
            dmenu = "${pkgs.rofi}/bin/rofi -p dunst -dmenu";
            browser = "${pkgs.firefox}/bin/firefox -new-tab";
            icon_position = "left";
            max_icon_size = 80;
            frame_width = 2;
            frame_color = my-theme.color5;
            corner_radius = 2;
          };
          shortcuts = { close = "esc"; };
          urgency_low = {
            frame_color = my-theme.color2;
            foreground = my-theme.color7;
            background = my-theme.color10;
            timeout = 8;
          };
          urgency_normal = {
            frame_color = my-theme.color4;
            foreground = my-theme.color7;
            background = my-theme.color10;
            timeout = 8;
          };
          urgency_critical = {
            frame_color = my-theme.color1;
            foreground = my-theme.color7;
            background = my-theme.color10;
            timeout = 8;
          };
        };
        iconTheme = {
          name = "Papirus-Dark";
          package = pkgs.papirus-icon-theme;
          size = "128x128";
        };
      };

      xresources.properties = {
        "xft.dpi" = "144";
        "XTerm*faceName" = "dejavu sans mono";
        "Xcursor.size" = "32";
        "Xcursor.theme" = "Bibata Oil";
      };

      programs.vim = {
        enable = true;
        extraConfig = ''
          set number
          set linebreak
          set showbreak=+++
          set textwidth=100
          set showmatch
          set visualbell
          set hlsearch
          set smartcase
          set ignorecase
          set incsearch
          set autoindent
          set shiftwidth=4
          set smartindent
          set smarttab
          set softtabstop=4
          set ruler
          set undolevels=1000
          set backspace=indent,eol,start
        '';
      };

      programs.emacs = {
        enable = true;
        package = pkgs.emacs27;
      };

      programs.git = {
        delta.enable = true;
        enable = true;
        userName = "Roch D'Amour";
        userEmail = "roch.damour@gmail.com";
        extraConfig = { pull.rebase = false; };
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

      programs.fzf = {
        enable = true;
        enableZshIntegration = true;
        defaultCommand =
          ''${pkgs.fd}/bin/fd --follow --type f --exclude="'.git'" .'';
        defaultOptions = [ "--exact" "--cycle" "--layout=reverse" ];
        enableFishIntegration = false;
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
          vif = "vim $(fzf)";
          lla = "ls -lah";
          lt = "ls -larth";
          dstop = "docker stop $(docker ps -a -q)";
          dclean = "docker rm $(docker ps -a -q)";
          dclear = "docker rmi $(docker images -q)";
          open = "$FILEMANAGER";
          nixc = "sudo $EDITOR /etc/nixos/configuration.nix";
          nbs = "sudo nixos-rebuild switch";
          nbsu = "sudo nixos-rebuild switch --upgrade";
          wttr = "curl wttr.in";
          k = "kubectl";
          randpw =
            "dd if=/dev/urandom bs=1 count=64 2>/dev/null | base64 -w 0 | rev | cut -b 2- | rev";
          gitwtf = "echo 'git reset $(git merge-base master current)'";
          yolo =
            ''git commit -m "$(curl -s http://whatthecommit.com/index.txt)" '';
        };
        history = {
          ignoreSpace = true;
          extended = true;
          save = 50000;
        };
        # initExtra = "echo \"\\e[31mHello, friend.\\em \"";
      };

      programs.kitty = {
        enable = true;
        font.name = "Essential PragmataPro";
        settings = {
          font_size = "14.0";
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
          enabled_layouts = "Vertical";

          background = my-theme.color0;
          foreground = my-theme.color7;
          selection_background = my-theme.color7;
          selection_foreground = my-theme.color0;
          url_color = my-theme.color12;
          cursor = my-theme.color1;
          active_border_color = my-theme.color8;
          inactive_border_color = my-theme.color10;
          active_tab_background = my-theme.color12;
          active_tab_foreground = my-theme.color7;
          inactive_tab_background = my-theme.color8;
          inactive_tab_foreground = my-theme.color7;
          tab_bar_background = my-theme.color10;

          # normal
          color0 = my-theme.color0;
          color1 = my-theme.color1;
          color2 = my-theme.color2;
          color3 = my-theme.color3;
          color4 = my-theme.color4;
          color5 = my-theme.color5;
          color6 = my-theme.color6;
          color7 = my-theme.color7;

          # bright
          color8 = my-theme.color8;
          color9 = my-theme.color9;
          color10 = my-theme.color10;
          color11 = my-theme.color11;
          color12 = my-theme.color12;
          color13 = my-theme.color13;
          color14 = my-theme.color14;
          color15 = my-theme.color15;
        };

      };

      programs.zsh.oh-my-zsh = {
        enable = true;
        theme = "dpoggi";
        plugins = [
          "git"
          "git-flow"
          "git-extras"
          "pass"
          "docker"
          "docker-compose"
          "ansible"
          "terraform"
          "kubectl"
          "vagrant"
          "npm"
          "node"
          "python"
          "golang"
        ];
        extraConfig =
          "\n          export PATH=$HOME/bin:/usr/local/bin:$PATH\n          export PATH=$HOME/snap:$PATH\n          export PATH=$HOME/.emacs.d/bin/:$PATH\n        ";

      };

      gtk = {
        enable = true;
        iconTheme.package = pkgs.numix-icon-theme-square;
        iconTheme.name = "Numix-Square";
        theme.package = pkgs.amber-theme;
        theme.name = "Amber";
      };

    };
    useUserPackages = true;
    useGlobalPkgs = true;
  };

  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome3.enable = true;
  services.xserver.windowManager.stumpwm.enable = false;
  services.xserver.windowManager.xmonad.enable = false;
  services.xserver.windowManager.herbstluftwm.enable = true;
  services.xserver.windowManager.herbstluftwm.configFile =
    /etc/nixos/extras/herbstluftwm/autostart;
  services.xserver.layout = "ca,fr";

  services.xserver.dpi = 144;

  services.redshift = {
    enable = false;
    temperature.day = 6500;
    temperature.night = 3000;
  };

  location.latitude = 45.5;
  location.longitude = -73.5;

  environment.systemPackages = with pkgs; [
    qemu_kvm
    wget
    curl
    vim
    neovim
    git
    tig
    ack
    tree
    exa
    fd
    ripgrep
    bc
    bat
    finger_bsd
    gnupg
    pass
    zip
    unzip
    colordiff
    rsync
    htop
    gotop
    lsof
    pstree
    nmap
    parted
    ranger
    stow
    traceroute
    telnet
    tcpdump
    whois
    dnsutils
    mtr
    docker-compose
    vagrant
    xorg.xhost

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
    # master.discord
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
    # Gnome ext
    gnomeExtensions.dash-to-dock
    gnomeExtensions.caffeine
    gnomeExtensions.system-monitor
    gnomeExtensions.appindicator
    tldr
    nodejs-14_x
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
    cmake
    youtube-dl
    flameshot
    amber-theme
    python-language-server
    leiningen
    clojure
    kdenlive
    hunspell
    hunspellDicts.en-ca
    hunspellDicts.fr-any
    aspell
    aspellDicts.en
    aspellDicts.fr
    gitAndTools.delta
    dive
    gitlab-runner
    geogebra6
    picom
    sbcl
    pciutils
    # pkglist
    killall
    pdftk
    audacity
    libnotify
    vegeta
    jq
    nixfmt
  ];

  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [ dejavu_fonts opensans-ttf ];
  };

  # G-word (g*ming)
  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
  hardware.pulseaudio.support32Bit = true;

  # Keychron k8 fn keys stuff
  boot.extraModprobeConfig = ''
    options hid_apple fnmode=0
  '';

  boot.kernelPackages = pkgs.linuxPackages_latest;

  virtualisation.libvirtd.enable = true;
  virtualisation.docker.enable = true;

  services.udev.packages = [ pkgs.yubikey-personalization pkgs.libu2f-host ];
  services.pcscd.enable = true;
  services.printing.enable = true;
  services.printing.drivers = with pkgs; [ hplip ];

  # Fixes svg icon-theme
  # https://github.com/NixOS/nixpkgs/issues/13537#issuecomment-332327760
  # Use librsvg's gdk-pixbuf loader cache file as it enables gdk-pixbuf to load SVG files (important for icons)
  environment.sessionVariables = {
    GDK_PIXBUF_MODULE_FILE =
      "$(echo ${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/*/loaders.cache)";
  };
  environment.variables.EDITOR = "vim";

  services.cron = {
    enable = true;
    systemCronJobs = let
      dodocron =
        "notarock  XDG_RUNTIME_DIR=/run/user/$(id -u) notify-send BEDTIME 'Time to go to sleep'";
    in [
      "*/1 0-5 * * 0-5  ${dodocron}"
      "30/1 23 * * 0-5  ${dodocron}"
      "* 3 * * 5-6      ${dodocron}"
    ];
  };

}
