{ config, lib, pkgs, ... }:

{
  programs.kitty = {
    package = pkgs.runCommandLocal "" { } "mkdir $out";
    enable = true;
    font.name = "Essential PragmataPro";
    settings = {
      font_size = "11.0";
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

      background = config.myTheme.color0;
      foreground = config.myTheme.color7;
      selection_background = config.myTheme.color7;
      selection_foreground = config.myTheme.color0;
      url_color = config.myTheme.color12;
      cursor = config.myTheme.color1;
      active_border_color = config.myTheme.color8;
      inactive_border_color = config.myTheme.color10;
      active_tab_background = config.myTheme.color12;
      active_tab_foreground = config.myTheme.color7;
      inactive_tab_background = config.myTheme.color8;
      inactive_tab_foreground = config.myTheme.color7;
      tab_bar_background = config.myTheme.color10;

      # normal
      color0 = config.myTheme.color0;
      color1 = config.myTheme.color1;
      color2 = config.myTheme.color2;
      color3 = config.myTheme.color3;
      color4 = config.myTheme.color4;
      color5 = config.myTheme.color5;
      color6 = config.myTheme.color6;
      color7 = config.myTheme.color7;

      # bright
      color8 = config.myTheme.color8;
      color9 = config.myTheme.color9;
      color10 = config.myTheme.color10;
      color11 = config.myTheme.color11;
      color12 = config.myTheme.color12;
      color13 = config.myTheme.color13;
      color14 = config.myTheme.color14;
      color15 = config.myTheme.color15;
    };

  };

}
