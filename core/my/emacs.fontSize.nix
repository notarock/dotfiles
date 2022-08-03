{ config, lib, pkgs, ... }:

with lib;

{
  options.my.emacs.fontSize = mkOption {
    type = types.int;
    default = 16;
    description = "Emacs font size";
  };
}
