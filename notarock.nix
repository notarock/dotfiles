{ pkgs, ... }:

{
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.notarock = {
    home = "/home/notarock";
    description = "Sa Majesté";
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "docker" ]; # Enable ‘sudo’ for the user.

    packages = with pkgs; [
      #
      # Programs
      #
      kitty
      libreoffice
      evince
      gnome3.cheese
      gnome3.evolution
      gnome3.gedit
      gimp
      krita
      vlc
      obs-studio
      arandr
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
      moka-icon-theme
      papirus-icon-theme
      paper-icon-theme
      numix-solarized-gtk-theme
      numix-gtk-theme
      arc-theme
      gnome3.gnome-tweaks
      bibata-cursors
      capitaine-cursors

      #
      # Markup Languages
      #
      texlive.combined.scheme-full
      multimarkdown
      plantuml
      pandoc

      #
      # Programming Languages & associates tools
      #
      elixir # A cure for erlang
      #
      # PHP Stuff
      #
      php73
      php73Packages.composer
      # C stuff
      clang
      gcc
      gnumake
      cmake
      rtags # The power of the 80's
      # Go stuff
      go_bootstrap
      gotools
      gocode
      gotests # Gotta GO fast
      hugo
      # Haskell
      ghc
      hlint
      stack # Purely functionnal
      # Python stuff
      python38Full # SSSSSSSSSSSSSSsssssssssss
      python37Packages.pip
      # Parens
      sbcl # Steel bank's common lisp implementation
      clojure # Love this one.
      leiningen # Elegant weapons for a more... civilized age
      # Java
      maven
      # Others
      nodejs # Writing spaghetti code with 12gbs of dependencies
      plantuml # Uml and diagrams from text files
      rakudo # Perl 6
      nixfmt # Nixos configuration linter
      shellcheck # Shell script linter
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
    ];
  };
}
