{ config, lib, pkgs, ... }:

let
  my-theme = import ../../themes/base16-solarflare.nix;
  layoutFolder = "/etc/nixos/notarock/extras/hlwm-layouts";
  gapWidth = "25";
in {

  #########################################################################
  #   _   _           _         _   _        __ _                        #
  #  | | | | ___ _ __| |__  ___| |_| |_   _ / _| |___      ___ __ ___    #
  #  | |_| |/ _ \ '__| '_ \/ __| __| | | | | |_| __\ \ /\ / / '_ ` _ \   #
  #  |  _  |  __/ |  | |_) \__ \ |_| | |_| |  _| |_ \ V  V /| | | | | |  #
  #  |_| |_|\___|_|  |_.__/|___/\__|_|\__,_|_|  \__| \_/\_/ |_| |_| |_|  #
  #########################################################################

  xdg.configFile."herbstluftwm/autostart" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash

      hc() {
        ${pkgs.herbstluftwm}/bin/herbstclient "$@"
      }

      hc emit_hook reload

      xsetroot -solid '#000'

      # remove all existing keybindings
      hc keyunbind --all

      # keybindings
      # if you have a super key you will be much happier with Mod set to Mod4
      # Mod=Mod1    # Use alt as the main modifier
      Mod=Mod4   # Use the super key as the main modifier

      hc keybind $Mod-Shift-c quit
      hc keybind $Mod-Shift-r reload
      hc keybind $Mod-Shift-q close
      hc keybind Mod1-F4 close
      hc keybind Mod1-F2 spawn rofi -show run -lines 0
      hc keybind $Mod-Return spawn kitty # use your $TERMINAL with xterm as fallback
      hc keybind $Mod-d spawn rofi -show drun

      # For volume controls and stuff
      hc keybind XF86AudioRaiseVolume spawn ${pkgs.alsaUtils}/bin/amixer set -q Master 5%+
      hc keybind XF86AudioLowerVolume spawn ${pkgs.alsaUtils}/bin/amixer set -q Master 5%-
      hc keybind XF86AudioMute spawn ${pkgs.alsaUtils}/bin/amixer set Master toggle

      # Brightness using Light
      hc keybind XF86MonBrightnessUp spawn ${pkgs.light}/bin/light -A 5%
      hc keybind XF86MonBrightnessDown spawn ${pkgs.light}/bin/light -U 5%

      hc keybind $Mod-F3 spawn firefox
      hc keybind $Mod-Shift-e spawn emacsclient -c
      hc keybind $Mod-Shift-s spawn flameshot gui
      hc keybind $Mod-Insert spawn rofi-pass
      hc keybind $Mod-t spawn ~/.config/herbstluftwm/layout-menu
      hc keybind $Mod-Shift-0 spawn ~/.config/herbstluftwm/window-menu
      hc keybind $Mod-Shift-Return spawn ~/.config/herbstluftwm/scratchpad

      hc keybind $Mod-Shift-Home spawn ${pkgs.systemd}/bin/loginctl lock-session




      # basic movement
      # focusing clients
      hc keybind $Mod-Left  focus left
      hc keybind $Mod-Down  focus down
      hc keybind $Mod-Up    focus up
      hc keybind $Mod-Right focus right
      hc keybind $Mod-h     focus left
      hc keybind $Mod-j     focus down
      hc keybind $Mod-k     focus up
      hc keybind $Mod-l     focus right

      # moving clients
      hc keybind $Mod-Shift-Left  shift left
      hc keybind $Mod-Shift-Down  shift down
      hc keybind $Mod-Shift-Up    shift up
      hc keybind $Mod-Shift-Right shift right
      hc keybind $Mod-Shift-h     shift left
      hc keybind $Mod-Shift-j     shift down
      hc keybind $Mod-Shift-k     shift up
      hc keybind $Mod-Shift-l     shift right

      # splitting frames
      # create an empty frame at the specified direction
      hc keybind $Mod-u       split   bottom  0.5
      hc keybind $Mod-Shift-u chain \
                              . split bottom 0.5 \
                              . shift down \
                              . focus down

      hc keybind $Mod-o       split   right   0.5
      hc keybind $Mod-Shift-o chain \
                              . split right 0.5 \
                              . shift right \
                              . focus right

      # let the current frame explode into subframes
      hc keybind $Mod-Control-space split explode

      # resizing frames
      resizestep=0.05
      hc keybind $Mod-Control-h       resize left +$resizestep
      hc keybind $Mod-Control-j       resize down +$resizestep
      hc keybind $Mod-Control-k       resize up +$resizestep
      hc keybind $Mod-Control-l       resize right +$resizestep
      hc keybind $Mod-Control-Left    resize left +$resizestep
      hc keybind $Mod-Control-Down    resize down +$resizestep
      hc keybind $Mod-Control-Up      resize up +$resizestep
      hc keybind $Mod-Control-Right   resize right +$resizestep

      # tags
      tag_names=( {1..9} )
      tag_keys=( {1..9} 0 )

      hc rename default "''${tag_names[0]}" || true
      for i in "''${!tag_names[@]}" ; do
          hc add "''${tag_names[$i]}"
          key="''${tag_keys[$i]}"
          if ! [ -z "$key" ] ; then
              hc keybind "$Mod-$key" use_index "$i"
              hc keybind "$Mod-Shift-$key" move_index "$i"
          fi
      done

      # cycle through tags
      hc keybind $Mod-period use_index +1 --skip-visible
      hc keybind $Mod-comma  use_index -1 --skip-visible

      # layouting
      hc keybind $Mod-r remove
      hc keybind $Mod-s floating toggle
      hc keybind $Mod-f fullscreen toggle
      hc keybind $Mod-p pseudotile toggle

      # The following cycles through the available layouts within a frame, but skips
      # layouts, if the layout change wouldn't affect the actual window positions.
      # I.e. if there are two windows within a frame, the grid layout is skipped.
      hc keybind $Mod-space                                                           \
          or , and . compare tags.focus.curframe_wcount = 2                   \
          . cycle_layout +1 vertical horizontal max vertical grid    \
          , cycle_layout +1

      # mouse
      hc mouseunbind --all
      hc mousebind $Mod-Button1 move
      hc mousebind $Mod-Button2 zoom
      hc mousebind $Mod-Button3 resize

      # focus
      hc keybind $Mod-BackSpace   cycle_monitor
      hc keybind $Mod-Tab         cycle_all +1
      hc keybind $Mod-Shift-Tab   cycle_all -1
      hc keybind $Mod-c cycle
      hc keybind $Mod-i jumpto urgent

      SELECT="${my-theme.color3}"

      # theme
      hc attr theme.tiling.reset 1
      hc attr theme.floating.reset 1
      hc set frame_border_active_color '${my-theme.color10}'
      hc set frame_border_normal_color '${my-theme.color10}'
      hc set frame_bg_normal_color '${my-theme.color10}'
      hc set frame_bg_active_color $SELECT
      hc set frame_border_width 0
      hc set always_show_frame 0
      hc set frame_bg_transparent 1
      hc set frame_transparent_width 3
      hc set frame_gap 0

      hc attr theme.active.color $SELECT
      hc attr theme.normal.color '#000000'
      hc attr theme.urgent.color '${my-theme.color1}'
      hc attr theme.inner_width 0
      hc attr theme.inner_color black
      hc attr theme.border_width 2
      hc attr theme.floating.border_width 2
      hc attr theme.floating.outer_width 2
      hc attr theme.floating.outer_color black
      hc attr theme.active.inner_color $SELECT
      hc attr theme.active.outer_color $SELECT
      hc attr theme.background_color '#141414'

      hc set window_gap 0
      hc set frame_padding 0
      hc set smart_window_surroundings 0
      hc set smart_frame_surroundings 0
      hc set mouse_recenter_gap 0

      # rules
      hc unrule -F
      #hc rule class=XTerm tag=3 # move all xterms to tag 3
      hc rule focus=on # normally focus new clients
      #hc rule focus=off # normally do not focus new clients
      # give focus to most common terminals
      #hc rule class~'(.*[Rr]xvt.*|.*[Tt]erm|Konsole)' focus=on
      hc rule windowtype~'_NET_WM_WINDOW_TYPE_(DIALOG|UTILITY|SPLASH)' pseudotile=on
      hc rule windowtype='_NET_WM_WINDOW_TYPE_DIALOG' focus=on
      hc rule windowtype~'_NET_WM_WINDOW_TYPE_(NOTIFICATION|DOCK|DESKTOP)' manage=off

      # unlock, just to be sure
      hc unlock

      herbstclient set tree_style '╾│ ├└╼─┐'

      # do multi monitor setup here, e.g.:
      # hc set_monitors 1920x1080+0+0
      # or simply:
      hc detect_monitors

      # Kill Startup programs
      hc spawn pkill xsecurelock
      hc spawn pkill xss-lock
      hc spawn pkill polybar
      hc spawn pkill flameshot
      hc spawn pkill picom

      hc spawn ${pkgs.xss-lock}/bin/xss-lock \
          ${pkgs.coreutils}/bin/env \
          XSECURELOCK_AUTH_BACKGROUND_COLOR="${my-theme.color11}" \
          XSECURELOCK_PASSWORD_PROMPT=time \
          XSECURELOCK_AUTH_CURSOR_BLINK=0 \
          XSECURELOCK_NO_COMPOSITE=1 \
          XSECURELOCK_BLANK_DPMS_STATE=off \
          XSECURELOCK_BLANK_TIMEOUT=30 \
          ${pkgs.xsecurelock}/bin/xsecurelock

      # Startup programs
      hc spawn feh --bg-fill ~/Pictures/wallpaper/selected*
      hc spawn picom
      hc spawn flameshot
      hc spawn ${pkgs.polybar}/bin/polybar main;

      # gaps are based
      #   __ _  __ _ _ __  ___
      #  / _` |/ _` | '_ \/ __|
      # | (_| | (_| | |_) \__ \
      #  \__, |\__,_| .__/|___/
      #  |___/      |_|

      hc keybind $Mod-n set frame_gap 0
      hc keybind $Mod-g set frame_gap ${gapWidth}

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
}
