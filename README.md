# Dotfiles

Nix flake configurations for macOS and NixOS, with most user configuration
managed by Home Manager. `main` is the maintained branch; the historical
`macos` and `rebased-macos` branches use an older, disconnected layout.

## Configurations

| Output | Platform | Status |
| --- | --- | --- |
| `darwinConfigurations.Hectasio` | Apple Silicon macOS | Verified on this Mac |
| `darwinConfigurations.hectasio` | Apple Silicon macOS | Alias of Hectasio; not separately verified |
| `darwinConfigurations.coneorange` | Apple Silicon macOS | Evaluates; activation unverified |
| `nixosConfigurations.Kreizemm` | x86_64 Linux | Declared; not evaluated locally because it requires Linux |
| `nixosConfigurations.Zonnarth` | x86_64 Linux | Currently invalid: required user arguments are missing |
| `homeConfigurations.rdamour` | Standalone Home Manager | Currently invalid: its configuration omits `pkgs` |

The NixOS and standalone Home Manager outputs are retained as historical
machine configurations; do not treat them as supported until they are fixed
and tested.

## Hectasio

Run the following from the repository root:

```sh
make mac-check  # evaluate the default Hectasio configuration
make mac-build  # build without activating
make mac        # build and activate; prompts for sudo
```

Override the default host when evaluating another Darwin output:

```sh
make mac-check HOST=coneorange
```

Format Nix files with `make fmt`. Update pinned inputs with `make update`,
inspect the resulting `flake.lock` diff, verify the configuration, and commit
the lockfile separately. Nix generation rollback does not roll back Homebrew
applications.

## macOS fonts

Nix installs DejaVu, Open Sans, Font Awesome, IBM Plex, and Nerd Fonts Symbols.
Ghostty, Kitty, and Emacs use **Essential PragmataPro**, installed separately.

On a fresh Mac, install your licensed Essential PragmataPro Regular and Bold
TTF files through Font Book for the current user (`~/Library/Fonts`). Confirm
the family appears as `Essential PragmataPro`, then restart the terminals and
Emacs. The encrypted font copies under `secrets/` are not installed by the
current configuration.
