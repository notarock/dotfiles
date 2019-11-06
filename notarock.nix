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
      qtpass
      obs-studio
      feh
      gimp
      maven
      vlc
      rhythmbox
      gnome3.gedit
      #
      # Eye candy
      #
      moka-icon-theme
      papirus-icon-theme
      screenkey
      arc-icon-theme
      gnome3.gnome-tweaks

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

      # Devops tooling
      kubectl minikube kompose k9s
      google-cloud-sdk
    ];

  };

}
