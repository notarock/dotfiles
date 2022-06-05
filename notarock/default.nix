{ config, pkgs, inputs, ... }:

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

    users.root.programs.git = {
      enable = true;
      extraConfig.safe.directory = "/home/notarock/src/dotfiles";
    };

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
        ./stumpwm.nix
      ];

      xdg.mimeApps = {
        enable = true;
        defaultApplications = let
          pdf = [ "org.gnome.Evince.desktop" ];
          browser = [ "firefox.desktop" ];
          image = [ "org.nomacs.ImageLounge.desktop" ];
        in {
          "image/png" = image;
          "image/jpeg" = image;
          "image/pjpeg" = image;
          "image/bmp" = image;
          "image/gif" = image;
          "image/fif" = image;
          "application/pdf" = pdf;
          "text/html" = browser;
          "x-scheme-handler/http" = browser;
          "x-scheme-handler/https" = browser;
          "x-scheme-handler/about" = browser;
          "x-scheme-handler/slack" = [ "slack.desktop" ];
          "x-scheme-handler/zoommtg" = [ "us.zoom.Zoom.desktop" ];
        };
      };

      home.file.".background-image".source = let
        background = ../resources/bsd-grid.png;
        bgOut = "bgOut.png";
        wallpaper = pkgs.runCommandNoCC "wallpaper" { } ''
          mkdir -p $out/share;
          ${pkgs.imagemagick}/bin/convert ${background} \
          -fill "${config.myTheme.color4}" -opaque white \
          -fill "${config.myTheme.color0}" -opaque black ${bgOut} ;
          cp -Lr ${bgOut} $out/share;
        '';
      in "${wallpaper}/share/${bgOut}";

      home.activation = {
        setupPragmataProReg = config.lib.dag.entryAfter [ "writeBoundary" ] ''
          mkdir -p ~/.local/share/fonts
          ln -sf ${osConfig.sops.secrets.pragmatapro-reg.path} \
                    ~/.local/share/fonts/Essential\ PragmataPro-R_1.2.ttf
        '';
        setupPragmataProRegOTF =
          config.lib.dag.entryAfter [ "writeBoundary" ] ''
            mkdir -p ~/.local/share/fonts
            ln -sf ${osConfig.sops.secrets.pragmatapro-reg-otf.path} \
                      ~/.local/share/fonts/Essential\ PragmataPro-R_1.2.otf
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
        set-wallpaper = config.lib.dag.entryAfter [ "writeBoundary" ] ''
          feh --bg-tile ~/.background-image
        '';
      };

      myTheme = import ../themes/monokai-ristretto.nix;

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
        font.name = "IBM Plex Sans Text";
        font.package = pkgs.ibm-plex;
        font.size = 11;
        theme.package = pkgs.plata-theme;
        theme.name = "Plata";
      };

      home.keyboard.layout = "ca,fr";
      home.pointerCursor = {
        package = pkgs.bibata-cursors;
        name = "Bibata-Modern-Ice";
        size = 48;
      };

      programs = {
        go = {
          enable = true;
          package = pkgs.go_1_17;
          packages = {
            "golang.org/x/tools/cmd/goimports" = inputs.gotools;
            "golang.org/x/tools/cmd/godoc" = inputs.gotools;
            "golang.org/x/tools/cmd/gorename" = inputs.gotools;
            "golang.org/x/tools/cmd/guru" = inputs.gotools;
          };
        };

        command-not-found.enable = true;

        direnv = {
          enable = true;
          enableZshIntegration = true;
          nix-direnv = { enable = true; };
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
          plugins = with pkgs; [ rofi-emoji ];
          extraConfig = { dpi = osConfig.my.dpi; };
        };

        git = {
          difftastic = {
            enable = true;
            background = "light";
            color = "always";
          };
          delta.enable = false;
          enable = true;
          userName = "Roch D'Amour";
          userEmail = "roch.damour@gmail.com";
          extraConfig = { pull.rebase = false; };
        };
      };

      services = {
        volnoti = { enable = true; };
        cbatticon = {
          enable = true;
          commandCriticalLevel = ''
            notify-send "battery critical!"
          '';
        };
        nextcloud-client.enable = true;
        picom = {
          enable = true;
          shadow = false;
          vSync = true;
          experimentalBackends = true;
          fade = true;
          fadeDelta = 2;
        };
        flameshot = { enable = true; };
      };

      xresources.properties = {
        "xft.dpi" = toString osConfig.my.dpi;
        "XTerm*faceName" = "dejavu sans mono";
      };

    };
  };

}
