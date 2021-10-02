##
# dotfiles
#

build:
	nixos-rebuild switch --flake '.#' -v -L

fmt:
	nixfmt **/*.nix

update:
	nix flake update

# end
