{ config, pkgs, inputs, ... }:

let inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;
in (mkIf isLinux {
  # Wallpaper generator
  # home.file.".background-image".source = let
  #   background = ../resources/bsd-grid.png;
  #   bgOut = "bgOut.png";
  #   wallpaper = pkgs.runCommandNoCC "wallpaper" { } ''
  #     	mkdir -p $out/share;
  #     ${pkgs.imagemagick}/bin/convert ${background} \
  #     	-fill "${config.myTheme.color14}" -opaque white \
  #     	-fill "${config.myTheme.color0}" -opaque black ${bgOut} ;
  #     cp -Lr ${bgOut} $out/share;
  #   '';
  # in "${wallpaper}/share/${bgOut}";

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
})
