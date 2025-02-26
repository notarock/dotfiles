{ config, pkgs, inputs, lib, ... }:

let inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;
in (lib.mkIf isLinux {

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
