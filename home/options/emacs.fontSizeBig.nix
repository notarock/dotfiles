{ config, lib, pkgs, ... }:

with lib;

{
  options.my.emacs.fontSizeBig = mkOption {
    type = types.int;
    default = 28;
    description = "Emacs big font size";
  };
}
