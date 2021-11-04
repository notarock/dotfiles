{ config, pkgs, ... }:

{
  users.users.notarock = {
    isNormalUser = true;
    home = "/home/notarock";
    description = "Nickname for root";
    extraGroups = [ "wheel" "docker" "video" ];
    shell = pkgs.zsh;
    initialPassword = "Ch4ngeMoi%%%";
  };

  home-manager = {
    users.notarock = { pkgs, config, osConfig, ... }: {
      imports = [
        ./myTheme.nix
        ./packages.nix
        ./extras/herbstluftwm.nix
        ./extras/polybar.nix
        ./extras/udiskie.nix
        ./extras/dunst.nix
        ./extras/vim.nix
        ./extras/emacs.nix
        ./extras/zsh.nix
        ./extras/fzf.nix
        ./extras/kitty.nix
        ./extras/starship.nix
      ];

      home.activation = {
        setupPragmataProReg = config.lib.dag.entryAfter [ "writeBoundary" ] ''
              mkdir -p ~/.local/share/fonts
              ln -sf ${osConfig.sops.secrets.pragmatapro-reg.path} \
                        ~/.local/share/fonts/Essential\ PragmataPro-R_1.2.ttf
            '';
        setupPragmataProBold = config.lib.dag.entryAfter [ "writeBoundary" ] ''
              mkdir -p ~/.local/share/fonts
              ln -sf ${osConfig.sops.secrets.pragmatapro-bold.path} \
                        ~/.local/share/fonts/Essential\ PragmataPro-B_1.2.ttf
            '';
        wakatime-cfg = config.lib.dag.entryAfter [ "writeBoundary" ] ''
              ln -sf ${osConfig.sops.secrets.wakatime.path} \
                        ~/.wakatime.cfg
            '';
      };

      myTheme = import ../themes/base16-ia-dark.nix;

      manual = {
        html.enable = true;
        manpages.enable = true;
      };

      home = {
        stateVersion = "21.11";
        username = "notarock";
        packages = with pkgs; [ xss-lock xsecurelock ];
        enableNixpkgsReleaseCheck = true;
      };

      gtk = {
        enable = true;
        iconTheme.package = pkgs.numix-icon-theme-square;
        iconTheme.name = "Numix-Square";
        theme.package = pkgs.plata-theme;
        theme.name = "Plata";
      };

      xsession = {
        pointerCursor.package = pkgs.bibata-cursors;
        pointerCursor.name = "Bibata_Oil";
        pointerCursor.size = 48;
      };

      home.keyboard.layout = "ca,fr";

      programs = {
        command-not-found.enable = true;

        direnv = {
          enable = true;
          enableZshIntegration = true;
          nix-direnv = {
            enable = true;
            enableFlakes = true;
          };
        };

        exa = {
          enable = true;
          enableAliases = true;
        };

        feh.enable = true;

        gh = {
          enable = true;
          settings = {
            git_protocol = "ssh";

            prompt = "enabled";
          };
        };

        broot = {
          enable = true;
          enableZshIntegration = true;
        };

        rofi = {
          enable = true;
          # separator = "solid";
          font = "Essential PragmataPro 14";
          theme = "/etc/nixos/extras/rofi/conf";
          plugins = with pkgs; [
            rofi-emoji
          ];
          extraConfig = { dpi = 0; };
        };

        git = {
          delta.enable = true;
          enable = true;
          userName = "Roch D'Amour";
          userEmail = "roch.damour@gmail.com";
          extraConfig = { pull.rebase = false; };
        };
      };

      services = {
        nextcloud-client.enable = true;
        picom = {
          enable = true;
          shadow = false;
        };
      };

      xresources.properties = {
        "xft.dpi" = "144";
        "XTerm*faceName" = "dejavu sans mono";
      };

    };
  };

}
