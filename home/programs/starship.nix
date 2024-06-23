{ config, lib, pkgs, ... }:

{
  programs.starship = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
    enableFishIntegration = false;

    settings = {
      # This line replaces add_newline = false
      add_newline = false;
      format = "$nix_shell$username$hostname$directory$git_branch$git_commit$git_state$git_status$cmd_duration$jobs$status$character";

      cmd_duration = {
        disabled = true;
        min_time = 10000;
      };

      directory = {
        disabled = false;
        truncation_length = 5;
        style = "fg:purple";
        truncate_to_repo = false;
      };

      git_branch = {
        style = "fg:yellow";
        format = "[\\($branch\\)]($style) ";
      };

      username = {
        disabled = false;
        show_always = true;
        style_user = "fg:green";
        style_root = "bold fg:red";
        format = "[$user]($style)";
      };

      hostname = {
        ssh_only = false;
        style = "fg:cyan";
        format = "@[$hostname]($style):";
      };

      line_break = {
        disabled = true;
      };

      nix_shell = {
        impure_msg = "";
        pure_msg = "";
        symbol = "❄️";
        format = "[$symbol]($style)";
      };

      time = {
        disabled = true;
        format = "%H:%M";
      };
    };
  };
}
