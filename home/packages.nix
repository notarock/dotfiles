{ config, lib, pkgs, inputs, ... }:

{
  home.packages = with pkgs;
    let
      inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;
      commonPackages = [
        vscode-extensions.vscodevim.vim
        vscode

        wget
        curl
        ack
        tree
        fd
        ripgrep
        bc
        gnupg
        zip
        unzip
        rsync
        htop
        gotop
        lsof
        pstree
        inetutils
        tcpdump
        dnsutils
        mtr
        docker-compose
        xorg.xhost
        mage
        unrar
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
        evince
        transmission
        # postman
        # brogue
        neofetch
        kubectl
        minikube
        kompose
        k9s
        kubernetes-helm
        google-cloud-sdk
        # ansible

        nodePackages.prettier
        spotify
        ccls
        clang-tools
        wakatime
        texlive.combined.scheme-medium

        pandoc

        slack
        yt-dlp
        hunspell
        hunspellDicts.en-ca
        hunspellDicts.fr-any
        aspell
        aspellDicts.en
        aspellDicts.fr
        gitAndTools.delta
        dive
        delve
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
        ripgrep
        sqlite
        wordnet
        delve
        dmenu
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

        # Discord doesnt work? fix is where in this line lol
        # (import inputs.nixpkgs-discord { inherit (pkgs) config system; }).discord
        (import inputs.nixpkgs-stable { inherit (pkgs) config system; }).ansible
      ];
      linuxPackages = [
        _1password-cli
        _1password-gui
        rustdesk
        gnome-tweaks
        kdePackages.kdenlive
        lxappearance
        vlc
        libreoffice
        pomodoro

        finger_bsd
        parted
        #
        chromium
        firefox
        discord
        # docker-compose
        # xorg.xhost
        # mage
        # unrar
        woeusb
        # xorg.xmessage
        # pciutils
        # openconnect
        # openvpn
        vagrant
        gedit
        gimp
        krita
        obs-studio
        arandr
        peek
        scrot
        rhythmbox
        nomacs
        # minikube
        # kompose
        gnomeExtensions.dash-to-dock
        gnomeExtensions.caffeine
        gnomeExtensions.system-monitor
        gnomeExtensions.appindicator
        gnomeExtensions.paperwm
        # texlive.combined.scheme-medium
        zoom-us
        nitrogen
        # pdftk
        # nix-index
        # kdenlive
        # pdftk
        volumeicon
        networkmanager_dmenu
        # dmenu
        blueman
        (import inputs.nixpkgs-stable {
          inherit (pkgs) config system;
        }).screenkey
      ];
      darwinPackages = [
        # Add macOS-specific packages here
      ];
    in commonPackages ++ (if isDarwin then darwinPackages else linuxPackages);

}
