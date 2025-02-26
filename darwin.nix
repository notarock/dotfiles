{ config, pkgs, lib, ... }:

# defaults write com.microsoft.VSCode ApplePressAndHoldEnabled -bool false

let inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;

in (builtins.trace "Base system imports was set to Darwin" {

  nix.nixPath = [ "nixpkgs=${pkgs.path}" ];

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = [ pkgs.vim ];
  networking.hosts = {
    "192.168.2.64" = [ "r630.local" "traduir.notarock.lol" ];
  };

  fonts = {
    packages = with pkgs;
      [ dejavu_fonts open-sans font-awesome ibm-plex ]
      ++ builtins.filter lib.attrsets.isDerivation
      (builtins.attrValues pkgs.nerd-fonts);
  };

  environment.variables = { EDITOR = "vim"; };

  programs.zsh.enable = true;

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  nix = {
    gc.automatic = true;
    settings = {
      # package = pkgs.nix;
      max-jobs = lib.mkDefault 8;

      sandbox = false;
      extra-sandbox-paths = [
        "/System/Library/Frameworks"
        "/System/Library/PrivateFrameworks"
        "/usr/lib"
        "/private/tmp"
        "/private/var/tmp"
        "/usr/bin/env"
      ];
    };
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 5;

  system.keyboard.enableKeyMapping = true;

  system.defaults.NSGlobalDomain."com.apple.keyboard.fnState" = true;

  system.defaults.NSGlobalDomain.NSDocumentSaveNewDocumentsToCloud = false;

  system.defaults.NSGlobalDomain.NSNavPanelExpandedStateForSaveMode = true;
  system.defaults.NSGlobalDomain.NSNavPanelExpandedStateForSaveMode2 = true;

  system.defaults.dock.wvous-tr-corner = 12; # Top left -> Notification Center
  system.defaults.dock.wvous-tl-corner = 11; # Top Right -> Launchpad

  system.defaults.NSGlobalDomain.AppleInterfaceStyleSwitchesAutomatically =
    true; # Light / Dark modes

  system.defaults.NSGlobalDomain.AppleShowAllExtensions = true;
  system.defaults.finder = {
    ShowStatusBar = true;
    _FXShowPosixPathInTitle = true;
    ShowPathbar = true;
    FXPreferredViewStyle = "Nlsv";
    AppleShowAllExtensions = true;
  };

  system.defaults.".GlobalPreferences"."com.apple.sound.beep.sound" =
    "/System/Library/Sounds/Frog.aiff";

  system.defaults.dock.show-recents = false;
  services.skhd.enable = false;

  # Homebrew integration
  homebrew = {
    enable = true;
    # brewPrefix = "/usr/local/bin";
    brews = [ "java" ];
    onActivation = {
      cleanup = "zap";
      upgrade = true;
    };
    # taps = [
    #   "homebrew/cask"
    #   #      "homebrew/cask-drivers"
    # ];
    casks = [
      "obs"
      "gimp"
      "nextcloud"
      "krita"
      "visual-studio-code"
      "microsoft-office"
      # "yubico-yubikey-manager"
      "steam"
      "raycast"
      "1password"
      "discord"
      "spotify"
      "slack"
      "kitty"
      "rectangle"
      "firefox"
      "google-chrome"
      "lens"
      # "mpv"
      "yubico-authenticator"
      "ghostty"
    ];
  };

})
