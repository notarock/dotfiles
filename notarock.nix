
{ pkgs, ... }:

{
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.notarock = {
    home = "/home/notarock";
    description = "Sa Majesté";
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "docker" ]; # Enable ‘sudo’ for the user.
    packages = with pkgs; [
      kitty tilda
      libreoffice evince
      firefox thunderbird
      qtpass
      obs-studio
      #
      # Eye candy
      #
      moka-icon-theme screenkey
      texlive.combined.scheme-full
    ];
  };
}
