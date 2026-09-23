{
  config,
  pkgs,
  inputs,
  lib,
  ...
}:

let
  inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;
  flexokiGtk = pkgs.stdenvNoCC.mkDerivation {
    pname = "flexoki-gtk";
    version = "2026-03-07";
    src = pkgs.fetchFromGitHub {
      owner = "kepano";
      repo = "flexoki";
      rev = "8d723bac4a9ac46adfdf99d42155286977aac72a";
      hash = "sha256-IxnvoZ9hGEvwq/PBbHTL5L2a2kxMSXSINIfd5Dg9ttA=";
    };
    installPhase = ''
      mkdir -p $out/share/themes/flexoki
      cp -r gtk/. $out/share/themes/flexoki
    '';
  };
in
(lib.mkIf isLinux {

  gtk = {
    enable = true;
    iconTheme.package = pkgs.numix-icon-theme-square;
    iconTheme.name = "Numix-Square";
    font.name = "IBM Plex Sans Text";
    font.package = pkgs.ibm-plex;
    font.size = 11;
    theme.package = flexokiGtk;
    theme.name = "flexoki";
  };

  fonts.fontconfig.enable = true;

  home = {
    pointerCursor.x11.enable = true;
    pointerCursor.package = pkgs.bibata-cursors;
    pointerCursor.name = "Bibata_Oil";
    pointerCursor.size = 48;
  };
})
