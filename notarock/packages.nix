{ config, lib, pkgs, inputs, ... }:

{
  home.packages = with pkgs; [
    _1password
    _1password-gui

    monero-cli

   vscode-extensions.vscodevim.vim
   vscode
   

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
    # python
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
    gedit
    pomodoro
    gimp
    krita
    vlc
    obs-studio
    arandr
    peek
    transmission
    # postman
    chromium
    firefox
    brogue
    scrot
    rhythmbox
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

    # gnomeExtensions.dash-to-dock
    # gnomeExtensions.caffeine
    # gnomeExtensions.system-monitor
    # gnomeExtensions.appindicator
    # gnomeExtensions.paperwm

    nodePackages.prettier
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
    jetbrains.datagrip
    kotlin
    figlet
    lxappearance

    ripgrep
    sqlite
    wordnet
    delve
    volumeicon
    networkmanager_dmenu
    dmenu
    blueman

    awscli2

    # (retroarch.override {
    #   cores = with libretro; [
    #     bsnes-mercury
    #     mgba
    #     mupen64plus

    #     snes9x
    #     snes9x2010
    #     yabause
    #   ];
    # })

    (import inputs.nixpkgs-stable { inherit (pkgs) config system; }).ansible
    (import inputs.nixpkgs-stable { inherit (pkgs) config system; }).screenkey

    # Discord doesnt work? fix is where in this line lol
    # (import inputs.nixpkgs-discord { inherit (pkgs) config system; }).discord
    discord

  ];

}
