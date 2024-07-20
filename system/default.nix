{ config, lib, pkgs, ... }:

{
  system.stateVersion = "24.11";
  nix.nixPath = [ "nixpkgs=${pkgs.path}" ];

  imports = [
    ./enableFlake.nix
    ./keychron.nix
    ./services.nix
    ./system.nix
    ./systemPackages.nix
  ];
}
