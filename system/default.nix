{ config, lib, pkgs, ... }:

{
  imports = [
    ./enableFlake.nix
    ./keychron.nix
    ./my
    ./services.nix
    ./system.nix
    ./systemPackages.nix
  ];
}
