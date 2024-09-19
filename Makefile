##
# dotfiles
#

build:
	nixos-rebuild --use-remote-sudo switch --flake '.#' -v -L
	doom sync

mac:
	darwin-rebuild switch --flake '.#' -v -L
	doom sync

hm:
	nix run github:nix-community/home-manager --no-write-lock-file -- switch  --flake ~/src/dotfiles
	doom sync

fmt:
	nixfmt **/*.nix

update:
	nix flake update --commit-lock-file

# end
