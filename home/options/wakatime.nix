{ config, lib, pkgs, ... }:

with lib;

{
  options.my.wakatime.enable = mkOption {
    type = types.bool;
    default = 144;
    description = "Screen DPI used by the host";
  };
}
