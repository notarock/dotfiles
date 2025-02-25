{ nixosConfig, config, osConfig, lib, pkgs, ... }:

{
  home.activation = {
    #        setupPragmataProReg = config.lib.dag.entryAfter [ "writeBoundary" ] ''
    #          mkdir -p ~/.local/share/fonts
    #          ln -sf ${osConfig.sops.secrets.pragmatapro-reg.path} \
    #                    ~/.local/share/fonts/Essential\ PragmataPro-R.ttf
    #        '';
    #        setupPragmataProRegOTF = config.lib.dag.entryAfter [ "writeBoundary" ] ''
    #          mkdir -p ~/.local/share/fonts
    #          ln -sf ${osConfig.sops.secrets.pragmatapro-reg-otf.path} \
    #                    ~/.local/share/fonts/Essential\ PragmataPro-R.otf
    #        '';
    #        setupPragmataProBold = config.lib.dag.entryAfter [ "writeBoundary" ] ''
    #          mkdir -p ~/.local/share/fonts
    #          ln -sf ${osConfig.sops.secrets.pragmatapro-bold.path} \
    #                    ~/.local/share/fonts/Essential\ PragmataPro-B.ttf
    #        '';
    #        setupPragmataProBoldOTF = config.lib.dag.entryAfter [ "writeBoundary" ] ''
    #          mkdir -p ~/.local/share/fonts
    #          ln -sf ${osConfig.sops.secrets.pragmatapro-bold-otf.path} \
    #                    ~/.local/share/fonts/Essential\ PragmataPro-B.otf
    #        '';
    #        wakatime-cfg = config.lib.dag.entryAfter [ "writeBoundary" ] ''
    #          ln -sf ${osConfig.sops.secrets.wakatime.path} \
    #                    ~/.wakatime.cfg
    #        '';
    #        openai-cfg = config.lib.dag.entryAfter [ "writeBoundary" ] ''
    #          ln -sf ${osConfig.sops.secrets.openai.path} \
    #                    ~/.openai.txt
    #        '';
    #        set-wallpaper = config.lib.dag.entryAfter [ "writeBoundary" ] ''
    #          ${pkgs.feh}/bin/feh --bg-tile ~/.background-image || true
    #        '';
  };

}
