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

  xdg.configFile."stumpwm/storepath.lisp" = {
    executable = true;
    text = ''
      (in-package :stumpwm)

      (setf *rofi-path* "${pkgs.rofi}/bin/rofi")

      (run-shell-command "${pkgs.xss-lock}/bin/xss-lock ${pkgs.coreutils}/bin/env XSECURELOCK_AUTH_BACKGROUND_COLOR=\"${config.myTheme.color11}\" XSECURELOCK_PASSWORD_PROMPT=time XSECURELOCK_AUTH_CURSOR_BLINK=0 XSECURELOCK_NO_COMPOSITE=1 XSECURELOCK_BLANK_DPMS_STATE=off XSECURELOCK_BLANK_TIMEOUT=30 ${pkgs.xsecurelock}/bin/xsecurelock")

      ;; End of file
    '';
  };

}
