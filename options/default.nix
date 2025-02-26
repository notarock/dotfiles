{ config, lib, pkgs, ... }:

{
  imports = [
    ./dpi.nix
    ./emacs.fontSizeBig.nix
    ./emacs.fontSize.nix
    ./wakatime.nix
  ];
}
