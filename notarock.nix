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
      scrot
      kitty tilda
      libreoffice evince
      firefox thunderbird
      rofi
      qtpass rofi-pass
      obs-studio
      feh
      gimp
      maven
      vlc
      rhythmbox
      gnome3.gedit
      screenkey
      #
      # Eye candy
      #
      moka-icon-theme
      papirus-icon-theme
      arc-icon-theme
      paper-icon-theme
      paper-gtk-theme
      numix-solarized-gtk-theme
      gnome3.gnome-tweaks
      bibata-cursors
      capitaine-cursors

      # Markup Languages
      texlive.combined.scheme-full
      multimarkdown

      # Programming Languages & associates tools
      sbcl                                   # Steel bank's common lisp implementation
      elixir                                 # A cure for erlang
      php72                                  # Programmers Hate Php
      gcc gnumake cmake rtags                # The power of the 80's
      go_bootstrap gotools gocode gotests    # Gotta GO fast
      ghc hlint stack                        # Purely functionnal
      python38Full                           # SSSSSSSSSSSSSSsssssssssss
      nodejs                                 # Writing spaghetti code with 12gbs of dependencies
      plantuml                               # Uml and diagrams from text files
      rakudo                                 # Perl 6
      nixfmt                                 # Nixos configuration linter
      shellcheck                             # Shell script linter
      clojure leiningen                           # Elegant weapons for a more... civilized age
      python37Packages.pip

      # Devops tooling
      kubectl minikube kompose k9s
      terraform
      kubernetes-helm
      google-cloud-sdk
      ansible

      plantuml

    ];

  };

}
