##
# dotfiles
#

build:
	nixos-rebuild --use-remote-sudo switch --flake '.#' -v -L
	echo "doom sync ? nah"

mac:
	darwin-rebuild switch --flake '.#' -v -L
	# doom sync

hm:
	nix run github:nix-community/home-manager --no-write-lock-file -- switch  --flake ~/src/dotfiles
	doom sync

fmt:
	nixfmt **/*.nix

update:
	nix flake update --commit-lock-file --extra-experimental-features nix-command --extra-experimental-features flakes

# end
