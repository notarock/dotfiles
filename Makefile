##
# dotfiles
#

build:
	sudo nixos-rebuild switch --flake '.#' -v -L
	doom sync

fmt:
	nixfmt **/*.nix

update:
	nix flake update

# end
