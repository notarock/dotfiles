{ nixosConfig, config, osConfig, lib, pkgs, ... }:


let 
  inherit (pkgs.stdenv.hostPlatform) isLinux;
in (lib.mkIf isLinux {
  services.polybar = {
    enable = false;
    package = ((pkgs.polybar.overrideAttrs (old: {
      version = "3.6.2";
      buildInputs = old.buildInputs ++ (with pkgs; [ libuv ]);
      src = pkgs.fetchFromGitHub {
        owner = "polybar";
        repo = "polybar";
        rev = "3.6.2";
        sha256 = "sha256-mLAcA8afGLNhRRU/x/TngCMcSRXdEM5wKWoYZhezJqU=";
        fetchSubmodules = true;
      };
      cmakeFlags = [ "-DBUILD_CONFIG=no" ];
    })).override { });
    config = {
      "settings" = {
        # throttle-ms = 50;
        # throttle-limit = 5;
        screenchange-reload = true;
      };

      "global/wm" = {
        margin-top = 0;
        margin-bottom = 0;
      };

      "bar/main" = {
        enable-ipc = "true";
        height = 42;
        line-size = 2;
        border-size = 2;
        border-color = config.myTheme.color11;

        tray-position = "right";
        tray-detached = false;

        background = config.myTheme.color0;
        foreground = config.myTheme.color15;

        padding = 3;
        font-0 = "Essential PragmataPro:size=10";
        font-1 = "Font Awesome 5 Free Regular:pixelsize=10";
        font-2 = "Font Awesome 5 Free Solid:pixelsize=10";
        font-3 = "Font Awesome 5 Brands:pixelsize=10";

        modules-left = "ewmh"; # "ewmh info-docker";
        modules-center = "time";
        modules-right = "cpu memory pulseaudio wlan battery battery2";

        wm-restack = "generic";
        override-redirect = "false";
      };

      "module/ewmh" = {
        type = "internal/xworkspaces";
        format = "<label-state>";
        enable-click = "true";
        reverse-scroll = "false";

        label-active-underline = config.myTheme.color2;
        label-occupied-underline = config.myTheme.color6;
        label-empty-padding = 1;
        label-active-padding = 1;
        label-occupied-padding = 1;
      };

      "module/cpu" = {
        type = "internal/cpu";
        interval = 2;
        format-prefix = " ";
        format-padding = 2;
        format-foreground = config.myTheme.color2;
        label = "%percentage%%";
      };

      "module/memory" = {
        type = "internal/memory";
        interval = 2;
        format-padding = 2;
        format-prefix = " ";
        format-foreground = config.myTheme.color3;
        label = "%percentage_used%%";
      };

      "module/wlan" = {
        type = "internal/network";
        interface = "wlp3s0";
        interval = 3;
        format-connected-margin = 2;
        format-connected-foreground = config.myTheme.color4;

        format-connected = " <label-connected>";
        label-connected = "%essid%";

        format-disconnected = "<label-disconnected>";
        format-disconnected-margin = "2";
        format-disconnected-foreground = config.myTheme.color5;
        label-disconnected = "%ifname% disconnected";
      };

      "module/eth" = {
        type = "internal/network";
        interface = "eno1";
        interval = 3;

        format-connected-prefix = " ";
        format-connected-prefix-color1 = config.myTheme.color1;
        label-connected = "%local_ip%";

        format-disconnected = "";
        # ;format-disconnected = <label-disconnected>
        # ;format-disconnected-underline = ${self.format-connected-underline}
        # ;label-disconnected = %ifname% disconnected
        # ;label-disconnected-color1 = ${colors.color1-alt}
      };

      "module/time" = {
        type = "internal/date";
        interval = 10;
        format-padding = 3;

        time = "%H:%M";
        date = "%A %d %b";

        label = "%date%, %time%";
        label-padding = 2;
      };

      "module/pulseaudio" = {
        type = "internal/alsa";
        master-mixer = "Master";
        headphone-id = 9;
        format-volume-padding = 2;
        format-muted-padding = 2;
        label-volume-font = 3;
        label-muted = " Muted";
        label-muted-font = 3;
        ramp-volume-0 = "";
        ramp-volume-1 = "";
        ramp-volume-2 = "";

        format-volume-margin = 2;
        format-volume-foreground = config.myTheme.color6;
        format-volume = "<ramp-volume> <label-volume>";
        label-volume = "%percentage%%";
        use-ui-max = "false";
        interval = 5;

        label-muted-background = config.myTheme.color0;
        label-muted-foreground = config.myTheme.color7;
      };

      "module/powermenu" = {
        type = "custom/menu";

        expand-right = "true";

        format-spacing = 1;
        format-margin = 0;
        format-background = config.myTheme.color0;
        format-foreground = config.myTheme.color15;
        format-padding = 2;

        label-open = "";
        label-close = "";
        label-separator = "|";

        #; reboot
        menu-0-1 = "";
        menu-0-1-exec = "menu-open-2";
        #; poweroff
        menu-0-2 = "";
        menu-0-2-exec = "menu-open-3";
        #; logout
        menu-0-0 = "";
        menu-0-0-exec = "menu-open-1";

        menu-2-0 = "";
        menu-2-0-exec = "reboot";

        menu-3-0 = "";
        menu-3-0-exec = "poweroff";

        menu-1-0 = "";
        menu-1-0-exec = "";

      };

      "module/battery" = {
        type = "internal/battery";
        format-charging-margin = 2;
        format-charging-foreground = config.myTheme.color2;
        format-discharging-margin = 2;
        format-discharging-foreground = config.myTheme.color1;
        format-full-margin = 2;
        format-full-foreground = config.myTheme.color3;
        full-at = 99;
        time-format = "%H:%M";
        battery = "BAT0";
        adapter = "ADP0";
        format-charging = "<animation-charging> <label-charging>";
        label-charging = "%percentage%% (%time%)";
        # label-charging = "%percentage%%";
        format-discharging = "<ramp-capacity> <label-discharging>";
        # label-discharging = "%percentage%% (%time%)";
        label-discharging = "%percentage%%";
        format-full = "<label-full>";
        label-charging-underline = config.myTheme.color3;
        label-discharging-underline = config.myTheme.color3;

        # format-charging-underline = config.myTheme.color0;
        # format-discharging-underline = "#ffffff";
        format-full-prefix = " ";
        ramp-capacity-0 = "";
        ramp-capacity-1 = "";
        ramp-capacity-2 = "";
        ramp-capacity-3 = "";
        ramp-capacity-4 = "";

        ramp-capacity-0-foreground = config.myTheme.color0;
        ramp-capacity-foreground = config.myTheme.color15;
        bar-capacity-width = 10;

        animation-charging-0 = "";
        animation-charging-1 = "";
        animation-charging-2 = "";
        animation-charging-3 = "";
        animation-charging-4 = "";

        animation-charging-framerate = 750;

        label-font = 1;
      };
      "module/battery2" = {
        type = "internal/battery";
        format-charging-margin = 2;
        format-charging-foreground = config.myTheme.color2;
        format-discharging-margin = 2;
        format-discharging-foreground = config.myTheme.color1;
        format-full-margin = 2;
        format-full-foreground = config.myTheme.color3;
        full-at = 99;
        time-format = "%H:%M";
        battery = "BAT1";
        adapter = "ADP1";
        format-charging = "<animation-charging> <label-charging>";
        label-charging = "%percentage%% (%time%)";
        # label-charging = "%percentage%%";
        format-discharging = "<ramp-capacity> <label-discharging>";
        label-discharging = "%percentage%% (%time%)";
        # label-discharging = "%percentage%%";
        format-full = "<label-full>";
        label-charging-underline = config.myTheme.color3;
        label-discharging-underline = config.myTheme.color3;

        # format-charging-underline = config.myTheme.color0;
        # format-discharging-underline = "#ffffff";
        format-full-prefix = " ";
        ramp-capacity-0 = "";
        ramp-capacity-1 = "";
        ramp-capacity-2 = "";
        ramp-capacity-3 = "";
        ramp-capacity-4 = "";

        ramp-capacity-0-foreground = config.myTheme.color0;
        ramp-capacity-foreground = config.myTheme.color15;
        bar-capacity-width = 10;

        animation-charging-0 = "";
        animation-charging-1 = "";
        animation-charging-2 = "";
        animation-charging-3 = "";
        animation-charging-4 = "";

        animation-charging-framerate = 750;

        label-font = 1;
      };

      "module/info-docker" = {
        type = "custom/script";
        exec = "~/.config/polybar/scripts/info-docker";
        interval = 10;
        format-padding = 2;
        format-foreground = config.myTheme.color6;
      };
    };

    script = "sleep 5 && ${pkgs.polybar}/bin/polybar main";
  };

  xdg.configFile."polybar/scripts/info-docker" = {
    executable = true;
    text = ''
      #!/bin/sh

      STATUS="running restarting dead"

      for stat in $STATUS; do
          output="$output $(sudo docker ps -qf status="$stat" | ${pkgs.coreutils}/bin/wc -l) |"
      done

      echo "|$output"
    '';
  };
})

