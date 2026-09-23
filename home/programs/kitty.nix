{
  config,
  lib,
  pkgs,
  ...
}:

{
  xdg.configFile."kitty/dark-theme.auto.conf".text = ''
    foreground #CECDC3
    background #100F0F
    selection_foreground #CECDC3
    selection_background #403E3C
    cursor #CECDC3
    cursor_text_color #100F0F
    active_border_color #AF3029
    inactive_border_color #403E3C
    active_tab_foreground #CECDC3
    active_tab_background #403E3C
    inactive_tab_foreground #878580
    inactive_tab_background #282726
    color0 #100F0F
    color1 #AF3029
    color2 #66800B
    color3 #AD8301
    color4 #205EA6
    color5 #A02F6F
    color6 #24837B
    color7 #878580
    color8 #6F6E69
    color9 #D14D41
    color10 #879A39
    color11 #D0A215
    color12 #4385BE
    color13 #CE5D97
    color14 #3AA99F
    color15 #CECDC3
  '';

  xdg.configFile."kitty/light-theme.auto.conf".text = ''
    foreground #100F0F
    background #FFFCF0
    selection_foreground #100F0F
    selection_background #CECDC3
    cursor #100F0F
    cursor_text_color #FFFCF0
    active_border_color #D14D41
    inactive_border_color #CECDC3
    active_tab_foreground #100F0F
    active_tab_background #CECDC3
    inactive_tab_foreground #6F6E69
    inactive_tab_background #E6E4D9
    color0 #100F0F
    color1 #D14D41
    color2 #879A39
    color3 #D0A215
    color4 #4385BE
    color5 #CE5D97
    color6 #3AA99F
    color7 #FFFCF0
    color8 #6F6E69
    color9 #AF3029
    color10 #66800B
    color11 #AD8301
    color12 #205EA6
    color13 #A02F6F
    color14 #24837B
    color15 #F2F0E5
  '';

  xdg.configFile."kitty/no-preference-theme.auto.conf".source =
    config.xdg.configFile."kitty/dark-theme.auto.conf".source;

  programs.kitty = {
    package = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin null;
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
      copy_on_select = "no";
      mouse_hide_wait = "3.0";
      sync_to_monitor = "yes";
      enabled_layouts = "Vertical";
    };
  };
}
