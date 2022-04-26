{ config, lib, pkgs, inputs, ... }:

{
  home.packages = with pkgs; [

    wget
    curl
    ack
    tree
    fd
    ripgrep
    bc
    finger_bsd
    gnupg
    zip
    unzip
    rsync
    htop
    gotop
    lsof
    pstree
    parted
    inetutils
    tcpdump
    dnsutils
    mtr
    docker-compose
    xorg.xhost
    mage
    unrar
    woeusb
    tldr
    xorg.xmessage
    librsvg
    gnumake
    cmake
    pciutils
    killall
    libnotify
    gcc
    pkg-config
    openconnect
    jre
    python
    openvpn
    # dbeaver
    git
    tig
    bat
    pass
    colordiff
    nmap
    vagrant
    libreoffice
    evince
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
    rhythmbox
    screenkey
    scrot
    neofetch
    nomacs

    kubectl
    minikube
    kompose
    k9s
    kubernetes-helm
    google-cloud-sdk
    # ansible

    gnomeExtensions.dash-to-dock
    gnomeExtensions.caffeine
    gnomeExtensions.system-monitor
    gnomeExtensions.appindicator
    gnomeExtensions.paperwm

    nodePackages.prettier
    nodejs-16_x
    spotify
    ccls
    clang-tools
    wakatime
    texlive.combined.scheme-medium

    pandoc

    zoom-us
    slack
    nitrogen
    youtube-dl
    hunspell
    hunspellDicts.en-ca
    hunspellDicts.fr-any
    aspell
    aspellDicts.en
    aspellDicts.fr
    gitAndTools.delta
    dive
    delve
    kdenlive
    pdftk
    jq
    nixfmt
    nix-index
    asciinema
    tmux
    audacity
    jetbrains.idea-ultimate
    jetbrains.goland
    kotlin
    figlet
    lxappearance

    ripgrep
    sqlite
    wordnet
    delve
    volumeicon
    networkmanager_dmenu
    blueman

    cheminot-ets
    awscli2

    (retroarch.override {
      cores = with libretro; [
        bsnes-mercury
        beetle-snes
        mgba
        mupen64plus

        snes9x
        snes9x2010
        yabause
      ];
    })

    # Discord doesnt work? fix is where in this line lol
    (import inputs.nixpkgs-stable { inherit (pkgs) config system; }).ansible
    # (import inputs.nixpkgs-discord { inherit (pkgs) config system; }).discord
    discord

    (steam.override {
      extraPkgs = pkgs: [ mono gtk3 gtk3-x11 libgdiplus zlib ];
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

}
