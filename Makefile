##
# dotfiles
#

HOST ?= Hectasio

.PHONY: mac-check mac-build mac fmt update

build:
	nixos-rebuild --use-remote-sudo switch --flake '.#' -v -L
	echo "doom sync ? nah"

mac-check:
	nix eval --no-write-lock-file '.#darwinConfigurations.$(HOST).system.drvPath'

mac-build:
	darwin-rebuild build --flake '.#$(HOST)' -v -L

mac:
	sudo darwin-rebuild switch --flake '.#$(HOST)' -v -L

hm:
	nix run github:nix-community/home-manager --no-write-lock-file -- switch  --flake ~/src/dotfiles
	doom sync

fmt:
	nix fmt

update:
	nix flake update --extra-experimental-features nix-command --extra-experimental-features flakes

# end
