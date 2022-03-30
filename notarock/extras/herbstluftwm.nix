{ config, lib, pkgs, ... }:

let
  layoutFolder = "/etc/nixos/notarock/extras/hlwm-layouts";
  gapWidth = "10";
  tags = [ "1" "2" "3" "4" "5" "6" "7" "8" "9" ];
in {

  #########################################################################
  #   _   _           _         _   _        __ _                        #
  #  | | | | ___ _ __| |__  ___| |_| |_   _ / _| |___      ___ __ ___    #
  #  | |_| |/ _ \ '__| '_ \/ __| __| | | | | |_| __\ \ /\ / / '_ ` _ \   #
  #  |  _  |  __/ |  | |_) \__ \ |_| | |_| |  _| |_ \ V  V /| | | | | |  #
  #  |_| |_|\___|_|  |_.__/|___/\__|_|\__,_|_|  \__| \_/\_/ |_| |_| |_|  #
  #########################################################################

  xsession.windowManager.herbstluftwm = {
    enable = true;
    tags = tags;
    settings = {
      frame_border_active_color = config.myTheme.color10;
      frame_gap = gapWidth;
    };
    keybinds = { Mod4-Return = "spawn ${pkgs.kitty}/bin/kitty"; };
    extraConfig = ''
      xsetroot -solid '#000'

      # keybindings
      # Mod=Mod1    # Use alt as the main modifier
      Mod=Mod4   # Use the super key as the main modifier

      herbstclient keybind $Mod-Shift-c quit
      herbstclient keybind $Mod-Shift-r reload
      herbstclient keybind $Mod-Shift-q close
      herbstclient keybind Mod1-F4 close
      herbstclient keybind Mod1-F2 spawn rofi -show run -lines 0
      herbstclient keybind $Mod-d spawn rofi -show drun
      herbstclient keybind $Mod-e spawn rofi -show emoji -modi emoji

      # For volume controls and mute
      herbstclient keybind XF86AudioRaiseVolume spawn ${pkgs.alsaUtils}/bin/amixer set -q Master 5%+
      herbstclient keybind XF86AudioLowerVolume spawn ${pkgs.alsaUtils}/bin/amixer set -q Master 5%-
      herbstclient keybind XF86AudioMute spawn ${pkgs.alsaUtils}/bin/amixer set Master toggle

      # Brightness using Light
      herbstclient keybind XF86MonBrightnessUp spawn ${pkgs.light}/bin/light -A 5%
      herbstclient keybind XF86MonBrightnessDown spawn ${pkgs.light}/bin/light -U 5%

      herbstclient keybind $Mod-F3 spawn firefox
      herbstclient keybind $Mod-Shift-e spawn emacs
      herbstclient keybind $Mod-Shift-s spawn flameshot gui
      herbstclient keybind $Mod-Insert spawn ${pkgs.rofi-pass}/bin/rofi-pass
      herbstclient keybind $Mod-t spawn ~/.config/herbstluftwm/layout-menu
      herbstclient keybind $Mod-Shift-0 spawn ~/.config/herbstluftwm/window-menu
      herbstclient keybind $Mod-Shift-Return spawn ~/.config/herbstluftwm/scratchpad
      herbstclient keybind $Mod-Shift-y spawn ~/.config/herbstluftwm/sticky

      herbstclient keybind $Mod-Shift-Home spawn ${pkgs.systemd}/bin/loginctl lock-session

      # basic movement
      # focusing clients
      herbstclient keybind $Mod-Left  focus left
      herbstclient keybind $Mod-Down  focus down
      herbstclient keybind $Mod-Up    focus up
      herbstclient keybind $Mod-Right focus right
      herbstclient keybind $Mod-h     focus left
      herbstclient keybind $Mod-j     focus down
      herbstclient keybind $Mod-k     focus up
      herbstclient keybind $Mod-l     focus right

      # moving clients
      herbstclient keybind $Mod-Shift-Left  shift left
      herbstclient keybind $Mod-Shift-Down  shift down
      herbstclient keybind $Mod-Shift-Up    shift up
      herbstclient keybind $Mod-Shift-Right shift right
      herbstclient keybind $Mod-Shift-h     shift left
      herbstclient keybind $Mod-Shift-j     shift down
      herbstclient keybind $Mod-Shift-k     shift up
      herbstclient keybind $Mod-Shift-l     shift right

      # splitting frames
      # create an empty frame at the specified direction
      herbstclient keybind $Mod-u       split   bottom  0.5
      herbstclient keybind $Mod-Shift-u chain \
                              . split bottom 0.5 \
                              . shift down \
                              . focus down

      herbstclient keybind $Mod-o       split   right   0.5
      herbstclient keybind $Mod-Shift-o chain \
                              . split right 0.5 \
                              . shift right \
                              . focus right

      # let the current frame explode into subframes
      herbstclient keybind $Mod-Control-space split explode

      # resizing frames
      resizestep=0.05
      herbstclient keybind $Mod-Control-h       resize left +$resizestep
      herbstclient keybind $Mod-Control-j       resize down +$resizestep
      herbstclient keybind $Mod-Control-k       resize up +$resizestep
      herbstclient keybind $Mod-Control-l       resize right +$resizestep
      herbstclient keybind $Mod-Control-Left    resize left +$resizestep
      herbstclient keybind $Mod-Control-Down    resize down +$resizestep
      herbstclient keybind $Mod-Control-Up      resize up +$resizestep
      herbstclient keybind $Mod-Control-Right   resize right +$resizestep

      # tags
      tag_names=( {1..9} )
      tag_keys=( {1..9} 0 )

      herbstclient rename default "''${tag_names[0]}" || true
      for i in "''${!tag_names[@]}" ; do
          # herbstclient add "''${tag_names[$i]}"
          key="''${tag_keys[$i]}"
          if ! [ -z "$key" ] ; then
              herbstclient keybind "$Mod-$key" use_index "$i"
              herbstclient keybind "$Mod-Shift-$key" move_index "$i"
          fi
      done

      # cycle through tags
      herbstclient keybind $Mod-period use_index +1 --skip-visible
      herbstclient keybind $Mod-comma  use_index -1 --skip-visible

      # layouting
      herbstclient keybind $Mod-r remove
      herbstclient keybind $Mod-s floating toggle
      herbstclient keybind $Mod-f fullscreen toggle
      herbstclient keybind $Mod-p pseudotile toggle
      herbstclient keybind $Mod-Shift-f set_attr clients.focus.floating toggle

      # The following cycles through the available layouts within a frame, but skips
      # layouts, if the layout change wouldn't affect the actual window positions.
      # I.e. if there are two windows within a frame, the grid layout is skipped.
      herbstclient keybind $Mod-space                                                           \
          or , and . compare tags.focus.curframe_wcount = 2                   \
          . cycle_layout +1 vertical horizontal max vertical grid    \
          , cycle_layout +1

      # mouse
      herbstclient mouseunbind --all
      herbstclient mousebind $Mod-Button1 move
      herbstclient mousebind $Mod-Button2 zoom
      herbstclient mousebind $Mod-Button3 resize

      # focus
      herbstclient keybind $Mod-BackSpace   cycle_monitor
      herbstclient keybind $Mod-Tab         cycle_all +1
      herbstclient keybind $Mod-Shift-Tab   cycle_all -1
      herbstclient keybind $Mod-c cycle
      herbstclient keybind $Mod-i jumpto urgent

      SELECT="${config.myTheme.color3}"

      # theme
      herbstclient attr theme.tiling.reset 1
      herbstclient attr theme.floating.reset 1
      herbstclient set frame_border_active_color '${config.myTheme.color10}'
      herbstclient set frame_border_normal_color '${config.myTheme.color10}'
      herbstclient set frame_bg_normal_color '${config.myTheme.color10}'
      herbstclient set frame_bg_active_color $SELECT
      herbstclient set frame_border_width 0
      herbstclient set always_show_frame 0
      herbstclient set frame_bg_transparent 1
      herbstclient set frame_transparent_width 3

      herbstclient attr theme.active.color $SELECT
      herbstclient attr theme.normal.color '#000000'
      herbstclient attr theme.urgent.color '${config.myTheme.color1}'
      herbstclient attr theme.inner_width 0
      herbstclient attr theme.inner_color black
      herbstclient attr theme.border_width 1
      herbstclient attr theme.floating.border_width 1
      herbstclient attr theme.floating.outer_width 1
      herbstclient attr theme.floating.outer_color black
      herbstclient attr theme.active.inner_color $SELECT
      herbstclient attr theme.active.outer_color $SELECT
      herbstclient attr theme.background_color '#141414'

      herbstclient set window_gap 0
      herbstclient set frame_padding 0
      herbstclient set smart_window_surroundings 0
      herbstclient set smart_frame_surroundings 1
      herbstclient set mouse_recenter_gap 0
      herbstclient set default_frame_layout grid
      herbstclient set focus_follows_mouse 1

      # rules
      herbstclient unrule -F

      herbstclient rule focus=on # normally focus new clients

      # Workspace rules for common programs

      herbstclient rule class=Firefox tag=1 focus=on switchtag=on
      herbstclient rule class=Emacs tag=2 focus=on switchtag=on
      herbstclient rule class=Spotify tag=8 focus=on switchtag=on
      herbstclient rule class=discord tag=9 focus=on # move Discord to tag 9
      herbstclient rule class=Slack tag=9 focus=on # move Discord to tag 9

      herbstclient rule windowtype~'_NET_WM_WINDOW_TYPE_(DIALOG|UTILITY|SPLASH)' pseudotile=on
      herbstclient rule windowtype='_NET_WM_WINDOW_TYPE_DIALOG' focus=on
      herbstclient rule windowtype~'_NET_WM_WINDOW_TYPE_(NOTIFICATION|DOCK|DESKTOP)' manage=off

      # unlock, just to be sure
      herbstclient unlock

      herbstclient set tree_style '╾│ ├└╼─┐'

      # do multi monitor setup here, e.g.:
      # herbstclient set_monitors 1920x1080+0+0
      # or simply:
      herbstclient detect_monitors

      # Kill Startup programs
      herbstclient spawn pkill xsecurelock
      herbstclient spawn pkill xss-lock

      herbstclient spawn ${pkgs.xss-lock}/bin/xss-lock \
          ${pkgs.coreutils}/bin/env \
          XSECURELOCK_AUTH_BACKGROUND_COLOR="${config.myTheme.color11}" \
          XSECURELOCK_PASSWORD_PROMPT=time \
          XSECURELOCK_AUTH_CURSOR_BLINK=0 \
          XSECURELOCK_NO_COMPOSITE=1 \
          XSECURELOCK_BLANK_DPMS_STATE=off \
          XSECURELOCK_BLANK_TIMEOUT=30 \
          ${pkgs.xsecurelock}/bin/xsecurelock

      # gaps are based
      #   __ _  __ _ _ __  ___
      #  / _` |/ _` | '_ \/ __|
      # | (_| | (_| | |_) \__ \
      #  \__, |\__,_| .__/|___/
      #  |___/      |_|

      herbstclient keybind $Mod-n set frame_gap 0
      herbstclient keybind $Mod-g set frame_gap ${gapWidth}

      xsetroot -curser_name ${config.xsession.pointerCursor.name}

      sleep 4

      # Startup programs
      herbstclient spawn ${pkgs.discord}/bin/discord
      herbstclient spawn ${pkgs.feh}/bin/feh --bg-tile ~/.background-image

      herbstclient spawn ${pkgs.polybar}/bin/polybar main
    '';
  };

  ########################################
  #   _                       _          #
  #  | |   __ _ _  _ ___ _  _| |_ ___    #
  #  | |__/ _` | || / _ \ || |  _(_-<    #
  #  |____\__,_|\_, \___/\_,_|\__/__/    #
  #             |__/                     #
  ########################################

  xdg.configFile."herbstluftwm/layout-menu" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash

      layout=$(ls ~/.config/herbstluftwm/layouts | ${pkgs.rofi}/bin/rofi -dmenu)
      if [ $layout ];
      then
        ~/.config/herbstluftwm/layouts/$layout
      fi
    '';
  };

  xdg.configFile."herbstluftwm/layouts/reset" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash

      herbstclient chain \
          . remove \
          . remove \
          . remove \
          . remove \
          . remove \
          . remove
    '';
  };

  xdg.configFile."herbstluftwm/layouts/masterleft-side" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash

      herbstclient chain \
          . remove \
          . remove \
          . remove \
          . remove \
          . remove \
          . remove \
          . split right 0.67 \
    '';
  };

  xdg.configFile."herbstluftwm/layouts/masterleft-2side" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash

      herbstclient chain \
          . remove \
          . remove \
          . remove \
          . remove \
          . remove \
          . remove \
          . split right 0.67 \
          . focus right \
          . split bottom 0.67 \
          . focus left
    '';
  };

  xdg.configFile."herbstluftwm/layouts/masterleft-bot-side" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash

      herbstclient chain \
          . remove \
          . remove \
          . remove \
          . remove \
          . remove \
          . remove \
          . split right 0.67 \
          . split bottom 0.80 \
    '';
  };

  xdg.configFile."herbstluftwm/layouts/master-stack" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash

      herbstclient chain \
          . remove \
          . remove \
          . remove \
          . remove \
          . remove \
          . remove \
          . split right 0.75 \
          . focus right \
          . split bottom 0.50 \
          . split bottom 0.50 \
          . focus down \
          . focus down \
          . split bottom 0.50 \
          . focus left
    '';
  };

  xdg.configFile."herbstluftwm/window-menu" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      set -e

      # offer a window menu offering possible actions on that window like
      # moving to a different tag or toggling its fullscreen state

      action_list() {
          local a="$1"
          "$a" "Close" herbstclient close
          "$a" "Toggle fullscreen" herbstclient fullscreen toggle
          "$a" "Toggle pseudotile" herbstclient pseudotile toggle
          for tag in $(herbstclient complete 1 move) ; do
              "$a" "Move to tag $tag" herbstclient move "$tag"
          done
      }

      print_menu() {
          echo "$1"
      }

      title=$(herbstclient attr clients.focus.title)
      title=''${title//&/&amp;}
      rofiflags=(
          -p "herbstclient:"
          -mesg "<i>$title</i>"
          -columns 3
          -location 2
          -width 100
          -no-custom
            )
      result=$(action_list print_menu | ${pkgs.rofi}/bin/rofi -i -dmenu -m -2 "''${rofiflags[@]}")
      [ $? -ne 0 ] && exit 0

      exec_entry() {
          if [ "$1" = "$result" ] ; then
              shift
              "$@"
              exit 0
          fi
      }

      action_list exec_entry      '';
  };

  xdg.configFile."herbstluftwm/scratchpad" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash

      # a i3-like scratchpad for arbitrary applications.
      #
      # this lets a new monitor called "scratchpad" appear in from the top into the
      # current monitor. There the "scratchpad" will be shown (it will be created if
      # it doesn't exist yet). If the monitor already exists it is scrolled out of
      # the screen and removed again.
      #
      # Warning: this uses much resources because herbstclient is forked for each
      # animation step.
      #
      # If a tag name is supplied, this is used instead of the scratchpad

      tag="''${1:-s}"
      hc() { "''${herbstclient_command[@]:-herbstclient}" "$@" ;}

      mrect=( $(hc monitor_rect "" ) )

      width=''${mrect[2]}
      height=''${mrect[3]}

      rect=(
          $((width/2))
          $((height/2))
          $((''${mrect[0]}+(width/4)))
          $((''${mrect[1]}+(height/4)))
      )

      hc add "$tag"

      monitor=scratchpad

      exists=false
      if ! hc add_monitor $(printf "%dx%d%+d%+d" "''${rect[@]}") \
                          "$tag" $monitor 2> /dev/null ; then
          exists=true
      else
          # remember which monitor was focused previously
          hc chain \
              , new_attr string monitors.by-name."$monitor".my_prev_focus \
              , substitute M monitors.focus.index \
                  set_attr monitors.by-name."$monitor".my_prev_focus M
      fi

      show() {
          hc lock
          hc raise_monitor "$monitor"
          hc focus_monitor "$monitor"
          hc unlock
          hc lock_tag "$monitor"
      }

      hide() {
          # if q3terminal still is focused, then focus the previously focused monitor
          # (that mon which was focused when starting q3terminal)
          hc substitute M monitors.by-name."$monitor".my_prev_focus \
              and + compare monitors.focus.name = "$monitor" \
                  + focus_monitor M
          hc remove_monitor "$monitor"
      }

                [ $exists = true ] && hide || show
    '';
  };

  xdg.configFile."herbstluftwm/sticky" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash

      # a i3-like sticky for arbitrary applications.
      #
      # this lets a new monitor called "sticky" appear in from the top into the
      # current monitor. There the "sticky" will be shown (it will be created if
      # it doesn't exist yet). If the monitor already exists it is scrolled out of
      # the screen and removed again.
      #
      # Warning: this uses much resources because herbstclient is forked for each
      # animation step.
      #
      # If a tag name is supplied, this is used instead of the sticky

      tag="''${1:-°}"
      hc() { "''${herbstclient_command[@]:-herbstclient}" "$@" ;}

      mrect=( $(hc monitor_rect "" ) )

      width=''${mrect[2]}
      height=''${mrect[3]}

      rect=(
          $((width/3))
          $((height/2))
          $((''${mrect[0]}+(width-(width/3)-(width/30))))
          $((''${mrect[1]}+(height/25)))
             )

      hc add "$tag"

      monitor=sticky

      exists=false
      if ! hc add_monitor $(printf "%dx%d%+d%+d" "''${rect[@]}") \
                          "$tag" $monitor 2> /dev/null ; then
          exists=true
      else
          # remember which monitor was focused previously
          hc chain \
              , new_attr string monitors.by-name."$monitor".my_prev_focus \
              , substitute M monitors.focus.index \
                  set_attr monitors.by-name."$monitor".my_prev_focus M
      fi

      show() {
          hc lock
          hc raise_monitor "$monitor"
          hc focus_monitor "$monitor"
          hc unlock
          hc lock_tag "$monitor"
      }

      hide() {
          # if q3terminal still is focused, then focus the previously focused monitor
          # (that mon which was focused when starting q3terminal)
          hc substitute M monitors.by-name."$monitor".my_prev_focus \
              and + compare monitors.focus.name = "$monitor" \
                  + focus_monitor M
          hc remove_monitor "$monitor"
      }

          [ $exists = true ] && hide || show
    '';
  };

}
