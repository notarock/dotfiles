{ config, lib, pkgs, ... }:

with lib;

{
  options.my.dpi = mkOption {
    type = types.int;
    default = 144;
    description = "Screen DPI used by the host";
  };
}
