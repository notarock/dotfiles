{ config, lib, pkgs, ... }:

let
  my-theme = import ../theme.nix;
  gapWidth = "20";
in {
  xdg.configFile."herbstluftwm/autostart" = { executable = true; text = ''
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

# # For display controls - either laptop, home or launch arandr
# hc keybind $Mod+F10 spawn ~/.config/herbstluftwm/rofi-display.sh
# # For brigthness control
# hc keybind $Mod+F9 spawn ~/.config/herbstluftwm/brightness.sh

# Keybinds for programs
# TODO: Revisit this. Have composable keybindings?

hc keybind $Mod-F3 spawn firefox
hc keybind $Mod-Shift-e spawn emacs
hc keybind $Mod-Shift-s spawn flameshot gui
hc keybind $Mod-Insert spawn rofi-pass

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
hc keybind $Mod-o       split   right   0.5
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
hc set frame_gap ${gapWidth}

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

# Startup programs

# Backgrounds and term colors
hc spawn feh --bg-fill ~/Pictures/wallpaper/selected*
hc spawn picom
hc keybind $Mod-n set frame_gap 0
hc keybind $Mod-g set frame_gap ${gapWidth}
  '';};

}
