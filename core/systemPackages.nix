{ config, lib, pkgs, inputs, ... }:

{
  environment.systemPackages = with pkgs; [
    openvpn
    dbeaver
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
    traceroute
    telnet
    tcpdump
    whois
    dnsutils
    mtr
    docker-compose
    vagrant
    xorg.xhost
    libreoffice
    evince
    gnome3.cheese
    gnome3.gedit
    gnome3.pomodoro
    gimp
    krita
    vlc
    obs-studio
    arandr
    peek
    transmission
    postman
    chromium
    firefox
    brogue
    steam
    minecraft
    scrot
    rofi
    rofi-pass
    feh
    rhythmbox
    screenkey
    scrot
    neofetch
    unrar
    gnome3.gnome-tweaks
    bibata-cursors
    capitaine-cursors
    element-desktop
    pkgs.numix-icon-theme-square
    pkgs.numix-gtk-theme
    woeusb
    jre
    kubectl
    minikube
    kompose
    k9s
    terraform
    kubernetes-helm
    google-cloud-sdk
    ansible
    gnomeExtensions.dash-to-dock
    gnomeExtensions.caffeine
    gnomeExtensions.system-monitor
    gnomeExtensions.appindicator
    tldr
    nodePackages.prettier
    nodejs-16_x
    spotify
    ccls
    clang-tools
    wakatime
    texlive.combined.scheme-medium
    go
    gomodifytags
    gocode
    gopls
    gore
    gotests
    goimports
    pandoc
    zoom-us
    yubioath-desktop
    # virt-manager
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
    juno-theme
    equilux-theme
    yaru-theme
    python38
    python38Packages.isort
    python38Packages.pytest
    python38Packages.black
    python38Packages.pyflakes
    python38Packages.pyaudio
    python38Packages.virtualenv
    python38Packages.wheel
    python38Packages.jupyter_core
    python38Packages.notebook
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
    killall
    pdftk
    audacity
    libnotify
    vegeta
    jq
    nixfmt
    gcc
    nix-index
    figlet
    asciinema
    tmux
    jetbrains.idea-community
    kotlin
    adoptopenjdk-openj9-bin-16

    (retroarch.override
    { cores = with libretro; [ bsnes-mercury beetle-snes mgba mupen64plus parallel-n64 snes9x snes9x2010 yabause ]; })
    python38Packages.python-language-server
    (import inputs.nixpkgs-discord {
        inherit (pkgs) config system;
    }).discord
    (steam.override {
      extraPkgs = pkgs: [ mono gtk3 gtk3-x11 libgdiplus zlib ];
      nativeOnly = true;
    }).run
    (pkgs.dwarf-fortress-packages.dwarf-fortress-full.override {
      theme = "cla";
      enableFPS = true;
      enableDwarfTherapist = false;
      enableSound = false;
      enableIntro = false;
      enableDFHack = true;
    })

  ];

  nixpkgs.overlays = [
    (final: prev: {
      nextcloud-client = (prev.nextcloud-client.overrideAttrs (o: rec {
        # does not find password
        # https://github.com/NixOS/nixpkgs/pull/113731
        # In case nextcloud will not remember passwords...
        qtWrapperArgs = [
          "--prefix LD_LIBRARY_PATH : ${
            prev.lib.makeLibraryPath [ prev.libsecret ]
          }"
        ];
      }));
    })
  ];

}
