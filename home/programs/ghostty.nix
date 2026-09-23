{
  config,
  lib,
  pkgs,
  ...
}:

{
  programs.ghostty = {
    enable = true;
    package = null;

    enableZshIntegration = true;
    settings = {
      theme = "dark:Flexoki Dark,light:Flexoki Light";

      font-size = 14;

      font-family = "Essential PragmataPro";
      font-family-bold = "Essential PragmataPro Bold";

      cursor-style = "block";
      cursor-style-blink = "false";

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

  };

}
