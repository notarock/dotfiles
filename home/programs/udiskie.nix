{ config, lib, pkgs, ... }:

let
  inherit (pkgs.stdenv.hostPlatform) isLinux;
in (lib.mkIf isLinux {
  services.udiskie = {
    enable = true;
    automount = true;
    notify = true;
    tray = "auto";
  };
})
