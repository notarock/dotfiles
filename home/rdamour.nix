{ config, pkgs, inputs, ... }:

{

  imports = [
    ../core/my/dpi.nix
    ../core/my/emacs.fontSize.nix
    ../core/my/emacs.fontSizeBig.nix

    ./myTheme.nix
    ./packages.nix
    ./extras/herbstluftwm.nix
    ./extras/polybar.nix
    ./extras/udiskie.nix
    # ./extras/dunst.nix
    ./extras/vim.nix
    ./extras/emacs.nix
    ./extras/zsh.nix
    ./extras/fzf.nix
    ./extras/kitty.nix
    ./extras/starship.nix
  ];

  targets.genericLinux.enable = true;
  programs.bash.enable = true;

  home.keyboard.layout = "ca,fr";

  xdg.mimeApps = {
    enable = false;
    defaultApplications = let
      pdf = [ "org.gnome.Evince.desktop" ];
      browser = [ "firefox.desktop" ];
      image = [ "org.nomacs.ImageLounge.desktop" ];
    in {
      "image/png" = image;
      "image/jpeg" = image;
      "image/pjpeg" = image;
      "image/bmp" = image;
      "image/gif" = image;
      "image/fif" = image;
      "application/pdf" = pdf;
      "text/html" = browser;
      "x-scheme-handler/http" = browser;
      "x-scheme-handler/https" = browser;
      "x-scheme-handler/about" = browser;
      "x-scheme-handler/slack" = [ "slack.desktop" ];
      "x-scheme-handler/zoommtg" = [ "us.zoom.Zoom.desktop" ];
    };
  };

  home.file.".background-image".source = let
    background = ../resources/bsd-grid.png;
    bgOut = "bgOut.png";
    wallpaper = pkgs.runCommandNoCC "wallpaper" { } ''
      	mkdir -p $out/share;
      ${pkgs.imagemagick}/bin/convert ${background} \
      	-fill "${config.myTheme.color14}" -opaque white \
      	-fill "${config.myTheme.color0}" -opaque black ${bgOut} ;
      cp -Lr ${bgOut} $out/share;
    '';
  in "${wallpaper}/share/${bgOut}";

  myTheme = import ../themes/base16-brewer.nix;

  manual = {
    html.enable = true;
    manpages.enable = true;
  };

  home = {
    username = "rdamour";
    homeDirectory = "/home/rdamour";
    packages = with pkgs; [ xss-lock xsecurelock ];
    enableNixpkgsReleaseCheck = true;
  };

  gtk = {
    enable = true;
    iconTheme.package = pkgs.numix-icon-theme-square;
    iconTheme.name = "Numix-Square";
    font.name = "IBM Plex Sans Text";
    font.package = pkgs.ibm-plex;
    font.size = 11;
    theme.package = pkgs.plata-theme;
    theme.name = "Plata";
  };

  fonts.fontconfig.enable = true;

  home = {
    pointerCursor.x11.enable = true;
    pointerCursor.package = pkgs.bibata-cursors;
    pointerCursor.name = "Bibata_Oil";
    pointerCursor.size = 48;
  };

  programs = {
    go = {
      enable = true;
      package = pkgs.go_1_19;
      packages = {
        "github.com/mdempsky/gocode" = inputs.gotools;
        "golang.org/x/tools/cmd/goimports" = inputs.gotools;
        "golang.org/x/tools/cmd/godoc" = inputs.gotools;
        "golang.org/x/tools/cmd/gorename" = inputs.gotools;
        "golang.org/x/tools/cmd/guru" = inputs.gotools;

        "github.com/motemen/gore/cmd/gore" = inputs.gore;
        "github.com/cweill/gotests/..." = inputs.gotests;
        "github.com/fatih/gomodifytags" = inputs.gomodifytags;
      };
    };

    command-not-found.enable = true;

    direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv = { enable = true; };
    };

    exa = {
      enable = true;
      enableAliases = true;
    };

    feh.enable = true;

    gh = {
      enable = true;
      settings = {
        git_protocol = "ssh";

        prompt = "enabled";
      };
    };

    broot = {
      enable = true;
      enableZshIntegration = true;
    };

    rofi = {
      enable = true;
      # separator = "solid";
      font = "Essential PragmataPro 14";
      # theme = "/etc/nixos/extras/rofi/conf";
      plugins = with pkgs; [ rofi-emoji ];
    };

    git = {
      enable = true;
      settings.user.name = "Roch D'Amour";
      extraConfig = {
        pull.rebase = true;
      };
    };
    delta.enable = false;
    diff-so-fancy.enable = true;
    difftastic.enable = false;
  };

  services = {
    picom = {
      enable = true;
      shadow = false;
      vSync = true;
    };
    flameshot = { enable = true; };
  };

  xresources.properties = { "XTerm*faceName" = "dejavu sans mono"; };

}
