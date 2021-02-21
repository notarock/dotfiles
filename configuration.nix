{ config, pkgs, ... }:

let
  hostSpecific = import ./host/variables.nix;
  my-theme = import ./users/notarock/theme.nix;
in {
  system.stateVersion = "20.09";

  imports = [ # Include the results of the hardware scan.
    # Contains host-specific configuration.
    # In order for this import to work, you have to link the desired
    # host folder to `host` in this repo's root. Like this:
    # ln -s hosts/zonnarth host
    <home-manager/nixos>
    ./core/system.nix
    ./core/services.nix
    ./host/hardware-configuration.nix
    ./users/notarock/user.nix
  ];

  programs = {
    zsh.enable = true;
    ssh.startAgent = false;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "gtk2";
    };
  };

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
  };

  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [ dejavu_fonts opensans-ttf ];
  };

  environment.variables.EDITOR = "vim";

  environment.systemPackages = with pkgs; [
    qemu_kvm wget curl vim neovim git tig ack tree exa fd ripgrep bc bat
    finger_bsd gnupg pass zip unzip colordiff rsync htop gotop lsof pstree nmap
    parted ranger stow traceroute telnet tcpdump whois dnsutils mtr
    docker-compose vagrant xorg.xhost libreoffice evince gnome3.cheese
    gnome3.evolution gnome3.gedit gnome3.pomodoro gimp krita vlc obs-studio
    arandr peek transmission postman chromium firefox brogue discord steam
    minecraft scrot rofi rofi-pass feh rhythmbox screenkey scrot neofetch unrar
    gnome3.gnome-tweaks bibata-cursors capitaine-cursors element-desktop
    nextcloud-client pkgs.numix-icon-theme-square pkgs.numix-gtk-theme woeusb
    jdk11 jre kubectl minikube kompose k9s terraform kubernetes-helm
    google-cloud-sdk ansible gnomeExtensions.dash-to-dock
    gnomeExtensions.caffeine gnomeExtensions.system-monitor
    gnomeExtensions.appindicator tldr nodejs-14_x spotify ccls clang-tools
    wakatime texlive.combined.scheme-medium go gocode gopls pandoc zoom-us
    yubioath-desktop virt-manager gnome3.dconf nitrogen xorg.xmessage ghc slack
    gdk-pixbuf librsvg gnumake cmake youtube-dl flameshot amber-theme
    python-language-server leiningen clojure kdenlive hunspell
    hunspellDicts.en-ca hunspellDicts.fr-any aspell aspellDicts.en
    aspellDicts.fr gitAndTools.delta dive gitlab-runner geogebra6 picom sbcl
    pciutils killall pdftk audacity libnotify vegeta jq nixfmt gcc nix-index
    (steam.override {
      extraPkgs = pkgs: [ mono gtk3 gtk3-x11 libgdiplus zlib ];
      nativeOnly = true;
    }).run
    (pkgs.dwarf-fortress-packages.dwarf-fortress-full.override {
      theme = "spacefox";
      enableFPS = false;
    })
  ];

  # Fixes svg icon-theme
  # https://github.com/NixOS/nixpkgs/issues/13537#issuecomment-332327760
  # Use librsvg's gdk-pixbuf loader cache file as it enables gdk-pixbuf to load SVG files (important for icons)
  environment.sessionVariables = {
    GDK_PIXBUF_MODULE_FILE =
      "$(echo ${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/*/loaders.cache)";
  };
  # Turns out it's quite annoying
  #
  # services.cron = {
  #   enable = true;
  #   systemCronJobs = let
  #     dodocron =
  #       "notarock  XDG_RUNTIME_DIR=/run/user/$(id -u) notify-send BEDTIME 'Time to go to sleep'";
  #   in [
  #     "*/1 0-5 * * 0-5  ${dodocron}"
  #     "30/1 23 * * 0-5  ${dodocron}"
  #     "* 3 * * 5-6      ${dodocron}"
  #   ];
  # };

}
