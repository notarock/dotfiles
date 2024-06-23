{ config, lib, pkgs, ... }:

{
  imports = [
    ./emacs.fontSizeBig.nix
    ./emacs.fontSize.nix
    ./wakatime.nix
  ];
}
