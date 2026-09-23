{ lib, pkgs, ... }:

{
  programs.opencode = {
    enable = true;
    extraPackages = [ pkgs.wakatime-cli ];

    settings = {
      model = "openai/gpt-6-sol";
      small_model = "openai/gpt-6-luna";
      share = "disabled";

      plugin = [
        "@dietrichgebert/ponytail@4.10.0"
        "@mohak34/opencode-notifier@0.3.0"
        "opencode-wakatime@1.3.9"
      ];

      agent = {
        plan = {
          model = "openai/gpt-6-astra";
          variant = "medium";
        };
        build = {
          model = "openai/gpt-6-sol";
          variant = "medium";
        };
        general = {
          model = "openai/gpt-6-sol";
          variant = "medium";
        };
        explore = {
          model = "openai/gpt-6-luna";
          variant = "medium";
        };
        title = {
          model = "openai/gpt-6-luna";
          variant = "low";
        };
        summary = {
          model = "openai/gpt-6-luna";
          variant = "low";
        };
        compaction = {
          model = "openai/gpt-6-sol";
          variant = "medium";
        };
      };

      compaction = {
        auto = true;
        prune = true;
        tail_turns = 8;
      };
      tool_output = {
        max_lines = 1000;
        max_bytes = 51200;
      };
    };

    tui.theme = "flexoki";
  };

  xdg.configFile."opencode/opencode-notifier.json".text = builtins.toJSON {
    sound = false;
    notification = true;
    suppressWhenFocused = true;
    minDuration = 10;
  };

  home.activation.backupUnmanagedOpenCodeConfig = lib.hm.dag.entryBefore [ "checkLinkTargets" ] ''
    path="$HOME/.config/opencode/opencode.json"
    if [ -e "$path" ] && [ ! -L "$path" ]; then
      if [ -e "$path.pre-nix" ]; then
        echo "Refusing to overwrite $path.pre-nix" >&2
        exit 1
      fi
      mv "$path" "$path.pre-nix"
    fi
  '';
}
