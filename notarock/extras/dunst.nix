{ config, lib, pkgs, ... }:

let
  my-theme = import ../theme.nix;
in {
  services.dunst = {
    enable = true;
    settings = {
      global = {
        font = "Essential PragmataPro 12";
        markup = "full";
        format = "<b>%s</b>\\n%b";
        sort = true;
        indicate_hidden = "yes";
        alignment = "left";
        bounce_freq = 0;
        show_age_threshold = -1;
        word_wrap = "yes";
        ignore_newline = "no";
        stack_duplicates = "yes";
        hide_duplicate_count = "yes";
        geometry = "0x5-250+50";
        shrink = "no";
        transparency = 20;
        idle_threshold = 0;
        monitor = 0;
        follow = "none";
        sticky_history = "yes";
        history_length = 15;
        show_indicators = "no";
        line_height = 3;
        separator_height = 5;
        padding = 25;
        horizontal_padding = 10;
        separator_color = "frame";
        startup_notification = "false";
        dmenu = "${pkgs.rofi}/bin/rofi -p dunst -dmenu";
        browser = "${pkgs.firefox}/bin/firefox --new-tab";
        icon_position = "left";
        text_icon_padding = 40;
        max_icon_size = 90;
        frame_width = 6;
        frame_color = my-theme.color5;
        corner_radius = 2;
      };
      shortcuts = { close = "esc"; };
      urgency_low = {
        frame_color = my-theme.color2;
        foreground = my-theme.color7;
        background = my-theme.color10;
        timeout = 8;
      };
      urgency_normal = {
        frame_color = my-theme.color4;
        foreground = my-theme.color7;
        background = my-theme.color10;
        timeout = 8;
      };
      urgency_critical = {
        frame_color = my-theme.color1;
        foreground = my-theme.color7;
        background = my-theme.color10;
        timeout = 8;
      };
    };
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.papirus-icon-theme;
      size = "128x128";
    };
  };
}
