{ config, lib, pkgs, ... }:

let
    hostSpecific = import ../../../host/variables.nix;
    my-theme = import ../theme.nix;
in {
      services.polybar = {
        enable = true;
        config = {
          "settings" = {
            throttle-ms = 50;
            throttle-limit = 5;
          };

          "bar/main" = {
            font-0 = "Essential PragmataPro:size=14";
            monitor = hostSpecific.mainMonitor;
            width = "100%";
            height = 30;
            radius = 0;
            top = true;
            # bottom = true;
            background = my-theme.color0;
            foreground = my-theme.color7;
            overline-size = 0;
            overline-color = my-theme.color6;
            underline-size = 2;
            underline-color = my-theme.color6;
            spacing = 1;
            padding-right = 2;
            module-margin-left = 0;
            module-margin-right = 2;
            modules-left = "ewmh";
            modules-center = "clock";
            modules-right = "volume cpu memory clock";
            tray-position = "right";
            tray-detached = false;
          };

          # "module/my-hostname" = {
          #   type = "custom/text";
          #   content = "notarock @ ${config.networking.hostName}";
          #   format-foreground = my-theme.color7;
          #   format-background = my-theme.color4;
          # };

          "module/cpu" = {
            type = "internal/cpu";
            interval = 2;
            format = "<label>";
            format-background = my-theme.color2;
            format-foreground = my-theme.color0;
            format-underline = my-theme.color2;
            format-overline = my-theme.color2;
            format-padding = 2;
            label = "CPU %percentage%%";
          };

          "module/memory" = {
            type = "internal/memory";
            format = "<label>";
            format-padding = 2;
            format-background = my-theme.color6;
            format-foreground = my-theme.color0;
            format-underline = my-theme.color6;
            format-overline = my-theme.color6;
            label = "RAM %percentage_used%%";
            label-font = 3;
          };

          "module/clock" = {
            type = "internal/date";
            date = "%%{T3}%Y-%m-%d %H:%M%%{T-}";
            format-padding = 2;
            format-background = my-theme.color1;
            format-foreground = my-theme.color0;
            format-underline = my-theme.color1;
            format-overline = my-theme.color1;
          };

          "module/volume" = {
            type = "internal/alsa";
            master-mixer = "Master";
            headphone-id = 9;
            format-volume-padding = 2;
            format-volume-background = my-theme.color3;
            format-volume-foreground = "#43433a";
            format-volume-underline = my-theme.color3;
            format-volume-overline = my-theme.color3;
            format-muted-padding = 2;
            format-muted-background = "#77ffff";
            format-muted-foreground = "#666666";
            label-volume = "volume %percentage%%";
            label-volume-font = 3;
            label-muted = "sound muted";
            label-muted-font = 3;
          };

          "module/ewmh" = {
            type = "internal/xworkspaces";
            enable-click = false;
            enable-scroll = false;
            label-active-foreground = "#ffffff";
            label-active-background = "#3f3f3f";
            label-active-padding = 4;
            label-empty-padding = 1;
          };
        };

        script = ''
          sleep 3 && USER=$(whoami); polybar main &
        '';
      };
}
