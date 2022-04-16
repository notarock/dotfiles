{ config, lib, pkgs, ... }:

{
  xdg.configFile."stumpwm" = {
    source = ./stumpwm;
    recursive = true;
  };

  xdg.configFile."stumpwm/colors.lisp" = {
    executable = true;
    text = ''
      (in-package :stumpwm)

      (setf *colors* (list "${config.myTheme.color0}"
                           "${config.myTheme.color1}"
                           "${config.myTheme.color2}"
                           "${config.myTheme.color3}"
                           "${config.myTheme.color4}"
                           "${config.myTheme.color5}"
                           "${config.myTheme.color6}"
                           "${config.myTheme.color7}"
                           "${config.myTheme.color8}"
                           "${config.myTheme.color9}"
                           "${config.myTheme.color10}"))
      ;; End of file
    '';
  };

}
