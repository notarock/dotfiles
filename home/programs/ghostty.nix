{ config, lib, pkgs, ... }:

{
  programs.ghostty = {
    enable = true;
    package = pkgs.emptyDirectory; # Explicitly override with null

    enableZshIntegration = true;
    settings = {
      theme = "notarock";

      font-size = 14;

      font-family = "Essential PragmataPro";
      font-family-bold = "Essential PragmataPro Bold";

      cursor-style = "block";
      cursor-style-blink = "false";

      background-opacity = 0.95;

      keybind = [
        "ctrl+w>enter=equalize_splits"

        "ctrl+w>v=new_split:right"
        "ctrl+w>s=new_split:down"
        "ctrl+w>d=close_surface"

        "ctrl+w>h=goto_split:left"
        "ctrl+w>j=goto_split:bottom"
        "ctrl+w>k=goto_split:top"
        "ctrl+w>l=goto_split:right"

        "ctrl+shift+enter=new_tab"

        "global:cmd+grave_accent=toggle_quick_terminal"
      ];
    };

    themes.notarock = {
      background = config.myTheme.color0;
      cursor-color = config.myTheme.color1;
      foreground = config.myTheme.color7;

      selection-background = config.myTheme.color7;
      selection-foreground = config.myTheme.color0;

      palette = [
        "0=${config.myTheme.color0}"
        "1=${config.myTheme.color1}"
        "2=${config.myTheme.color2}"
        "3=${config.myTheme.color3}"
        "4=${config.myTheme.color4}"
        "5=${config.myTheme.color5}"
        "6=${config.myTheme.color6}"
        "7=${config.myTheme.color7}"
        "8=${config.myTheme.color8}"
        "9=${config.myTheme.color9}"
        "10=${config.myTheme.color10}"
        "11=${config.myTheme.color11}"
        "12=${config.myTheme.color12}"
        "13=${config.myTheme.color13}"
        "14=${config.myTheme.color14}"
        "15=${config.myTheme.color15}"
      ];
    };
  };

}
