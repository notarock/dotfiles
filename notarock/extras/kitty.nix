
{ config, lib, pkgs, ... }:

let
  my-theme = import ../../themes/base16-brewer.nix;
in {
  programs.kitty = {
    enable = true;
    font.name = "Essential PragmataPro";
    settings = {
      font_size = "14.0";
      enable_audio_bell = false;
      open_url_with = "firefox";
      scrollback_lines = 5000;
      cursor_shape = "block";
      cursor_blink_interval = "1.0";
      cursor_stop_blinking_after = "1.0";
      cursor_text_color = "background";
      copy_on_select = "no";
      mouse_hide_wait = "3.0";
      sync_to_monitor = "yes";
      enabled_layouts = "Vertical";

      background = my-theme.color0;
      foreground = my-theme.color7;
      selection_background = my-theme.color7;
      selection_foreground = my-theme.color0;
      url_color = my-theme.color12;
      cursor = my-theme.color1;
      active_border_color = my-theme.color8;
      inactive_border_color = my-theme.color10;
      active_tab_background = my-theme.color12;
      active_tab_foreground = my-theme.color7;
      inactive_tab_background = my-theme.color8;
      inactive_tab_foreground = my-theme.color7;
      tab_bar_background = my-theme.color10;

      # normal
      color0 = my-theme.color0;
      color1 = my-theme.color1;
      color2 = my-theme.color2;
      color3 = my-theme.color3;
      color4 = my-theme.color4;
      color5 = my-theme.color5;
      color6 = my-theme.color6;
      color7 = my-theme.color7;

      # bright
      color8 = my-theme.color8;
      color9 = my-theme.color9;
      color10 = my-theme.color10;
      color11 = my-theme.color11;
      color12 = my-theme.color12;
      color13 = my-theme.color13;
      color14 = my-theme.color14;
      color15 = my-theme.color15;
    };

  };

}
