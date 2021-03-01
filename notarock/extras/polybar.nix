{ config, lib, pkgs, ... }:

let
    hostSpecific = import ../../hosts/kreizemm/variables.nix;
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
            font-1 = "FontAwesome:pixelsize=16";
            monitor = hostSpecific.mainMonitor;
            width = "100%";
            height = 30;
            radius = 0;
            top = true;
            background = my-theme.color0;
            foreground = my-theme.color15;
            overline-size = 0;
            overline-color = my-theme.color6;
            underline-size = 0;
            underline-color = my-theme.color6;
            spacing = 1;
            padding-right = 2;
            module-margin-left = 0;
            module-margin-right = 2;
            modules-left = "ewmh wired-network wireless-network";
            modules-center = "date";
            modules-right = "volume backlight cpu memory filesystem time";
            tray-position = "right";
            tray-detached = false;
          };

          "module/cpu" = {
            type = "internal/cpu";
            interval = 2;
            format = "<label>";
            format-background = my-theme.color10;
            format-foreground = my-theme.color15;
            format-underline = my-theme.color10;
            format-overline = my-theme.color10;
            format-padding = 2;
            label = " CPU %percentage%%";
          };

          "module/memory" = {
            type = "internal/memory";
            format-padding = 2;
            format-background = my-theme.color10;
            format-foreground = my-theme.color15;
            format-underline = my-theme.color10;
            format-overline = my-theme.color10;
            label = " RAM %percentage_used%%";
            label-font = 3;
          };

          "module/filesystem" = {
            type = "internal/fs";
            mount-0 = "/";
            interval = "60";
            fixed-values = "true";
            spacing = "4";
            format-padding = 2;
            label-mounted = " %free% free (%percentage_free%%) ";

            format-mounted-background = my-theme.color10;
            format-mounted-foreground = my-theme.color15;
            format-mounted-underline = my-theme.color10;
            format-mounted-overline = my-theme.color10;
          };

          "module/wired-network" = {
            format-padding = 2;
            type = "internal/network";
            interface = "enp7s0";
            label-connected = " %local_ip%%downspeed:9%%upspeed:9% ";
            label-connected-foreground = my-theme.color15;
            label-connected-background = my-theme.color10;

            label-disconnected = "not connected";
            label-disconnected-background = my-theme.color1;
            label-disconnected-foreground = my-theme.color0;

          };
          "module/backlight" = {
            format-padding = 2;
            enable-scroll = true;
            card = "intel_backlight";
            type = "internal/backlight";
            label = " %percentage%%";
            format-background = my-theme.color10;
            format-foreground = my-theme.color15;
            format-underline = my-theme.color10;
            format-overline = my-theme.color10;
          };

          "module/wireless-network" = {
            type = "internal/network";
            interface = "wlp3s0";
            format-padding = 2;
            label-connected = " %local_ip%%downspeed:9%%upspeed:9% %essid% ";
            label-connected-foreground = my-theme.color15;
            label-connected-background = my-theme.color10;
            label-disconnected = "not connected";
            label-disconnected-background = my-theme.color1;
            label-disconnected-foreground = my-theme.color0;
          };

          "module/date" = {
            type = "internal/date";
            date = "%%{T3}%Y-%m-%d";
            format-padding = 2;
            format-background = my-theme.color1;
            format-foreground = my-theme.color0;
            format-underline = my-theme.color1;
            format-overline = my-theme.color1;
            format = " <label>";
          };

          "module/time" = {
            type = "internal/date";
            date = "%H:%M%%{T-}";
            format-padding = 2;
            format-background = my-theme.color1;
            format-foreground = my-theme.color0;
            format-underline = my-theme.color1;
            format-overline = my-theme.color1;
            format = " <label>";
          };


          "module/volume" = {
            type = "internal/alsa";
            master-mixer = "Master";
            headphone-id = 9;
            format-volume-padding = 2;
            format-volume-background = my-theme.color10;
            format-volume-foreground = my-theme.color15;
            format-volume-underline = my-theme.color10;
            format-volume-overline = my-theme.color10;
            format-muted-padding = 2;
            format-muted-background = my-theme.color1;
            format-muted-foreground = my-theme.color0;
            format-volume = "<ramp-volume> <label-volume>";
            label-volume = "volume %percentage%%";
            label-volume-font = 3;
            label-muted = " Muted";
            label-muted-font = 3;
            ramp-volume-0 = "";
            ramp-volume-1 = "";
            ramp-volume-2 = "";
          };

          "module/ewmh" = {
            type = "internal/xworkspaces";
            enable-click = false;
            enable-scroll = false;
            label-active-foreground = my-theme.color15;
            label-active-background = my-theme.color12;
            label-active-padding = 3;
            label-empty-padding = 1;
          };
        };

        script = ''
          sleep 5 && USER=$(whoami); polybar main &
        '';
      };
}
