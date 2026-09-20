# My neglected dotfile repository, now with even more breakage!

This repository contains all of my configurations for doom-emacs,
stumpwm/herbstluftwm/whatever DE/WM I am using at the moment, and
many more. It is all managed by Nix, mostly home-manager. You will find a
"macos" branch with the most up-to-date changes. This is simply because I am
lazy and never merged the two system configurations properly...

## macOS fonts

Nix installs DejaVu, Open Sans, Font Awesome, IBM Plex, and Nerd Fonts Symbols.
Ghostty, Kitty, and Emacs use **Essential PragmataPro**, installed separately.

On a fresh Mac, install your licensed Essential PragmataPro Regular and Bold
TTF files through Font Book for the current user (`~/Library/Fonts`). Confirm
the family appears as `Essential PragmataPro`, then restart the terminals and
Emacs. The encrypted font copies under `secrets/` are not installed by the
current configuration.
