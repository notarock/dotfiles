{ config, lib, pkgs, ... }:

{
  xdg.mimeApps = {
    enable = true;
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

}
