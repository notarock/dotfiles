{ config, osConfig, lib, pkgs, inputs, ... }:

let
  DOOMLOCALDIR = "${config.xdg.dataHome}/doom";
  DOOMDIR = "${config.xdg.configHome}/doom";
  EMACSDIR = "${config.xdg.configHome}/emacs";
in {
  home.packages = with pkgs; [
    nixfmt

    shellcheck
    shfmt

    yaml-language-server
    editorconfig-checker
    editorconfig-core-c

    gopls
    gocode
    gomodifytags
    gotests
    gore

    nodejs-19_x

    python3
    nodePackages.pyright
    terraform

    haskellPackages.haskell-language-server
    haskellPackages.hoogle
    haskellPackages.cabal-install

    rustc
    cargo
    html-tidy
    nodePackages.stylelint
    nodePackages.js-beautify

    terraform-ls

    (pkgs.makeDesktopItem {
      name = "Emacs";
      exec = "emacs";
      comment = "Editor";
      desktopName = "Emacs";
      genericName = "Editor";
    })
  ];

  programs.emacs = { enable = true; };

  home.sessionVariables = { inherit DOOMLOCALDIR DOOMDIR; };
  systemd.user.sessionVariables = { inherit DOOMLOCALDIR DOOMDIR; };

  xdg.configFile."emacs" = {
    source = pkgs.applyPatches {
      name = "doom-emacs-source";
      src = inputs.doom-emacs;
    };
    recursive = true;
  };

  home.sessionPath = [ "${config.xdg.configHome}/emacs/bin" ];

  xdg.configFile."doom" = {
    recursive = true;
    source = ../doom.d;
    force = true;
  };

  xdg.configFile."doom/extra.el".text = ''
    (setq wakatime-cli-path "${pkgs.wakatime}/bin/wakatime-cli")

    (let ((font-family "Essential PragmataPro"))
      (setq doom-variable-pitch-font (font-spec :family "IBM Plex Sans Condensed")
            doom-font (font-spec :family font-family :size 14)
            doom-big-font (font-spec :family font-family :size 24)))
  '';

}
