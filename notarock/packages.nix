{ config, lib, pkgs, inputs, ... }:

{
  home.packages = with pkgs; [
    openvpn
    dbeaver
    git
    tig
    bat
    pass
    colordiff
    nmap
    vagrant

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
    rhythmbox
    screenkey
    scrot
    neofetch

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
    slack
    nitrogen
    youtube-dl
    flameshot
    hunspell
    hunspellDicts.en-ca
    hunspellDicts.fr-any
    aspell
    aspellDicts.en
    aspellDicts.fr
    gitAndTools.delta
    dive
    kdenlive
    pdftk
    jq
    nixfmt
    nix-index
    asciinema
    tmux
    audacity
    jetbrains.idea-community
    kotlin
    adoptopenjdk-openj9-bin-16
    figlet

    (retroarch.override {
      cores = with libretro; [
        bsnes-mercury
        beetle-snes
        mgba
        mupen64plus
        parallel-n64
        snes9x
        snes9x2010
        yabause
      ];
    })

    # Discord doesnt work? fix is where in this line lol
    (import inputs.nixpkgs-discord { inherit (pkgs) config system; }).discord

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
}
