{ config, lib, pkgs, ... }:

{
  systemd.services.keychron = {
    enable = true;
    description = "Activate Keychron K8 Fn keys";
    path = [ pkgs.coreutils ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.coreutils}/bin/echo 0 | ${pkgs.coreutils}/bin/tee /sys/module/hid_apple/parameters/fnmode";
    };
  };
}
