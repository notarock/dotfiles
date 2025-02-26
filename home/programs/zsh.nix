{ config, lib, pkgs, ... }:

{
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    # To keep using zsh with nix-shells
    plugins = [{
      name = "zsh-nix-shell";
      file = "nix-shell.plugin.zsh";
      src = pkgs.fetchFromGitHub {
        owner = "chisui";
        repo = "zsh-nix-shell";
        rev = "v0.1.0";
        sha256 = "0snhch9hfy83d4amkyxx33izvkhbwmindy0zjjk28hih1a9l2jmx";
      };
    }];
    shellAliases = {
      cp = "cp -i";
      df = "df -h";
      cdsrc = "cd ~/src/";
      ".." = "cd ..";
      "..." = "cd ../..";
      "...." = "cd ../../..";
      "....." = "cd ../../../..";
      vi = "vim";
      vif = "vim $(fzf)";
      dstop = "docker stop $(docker ps -a -q)";
      dclean = "docker rm $(docker ps -a -q)";
      dclear = "docker rmi --force $(docker images -q)";
      open = "$FILEMANAGER";
      nixc = "sudo $EDITOR /etc/nixos/configuration.nix";
      wttr = "curl wttr.in";
      k = "kubectl";
      randpw =
        "dd if=/dev/urandom bs=1 count=64 2>/dev/null | base64 -w 0 | rev | cut -b 2- | rev";
      gitwtf = "echo 'git reset $(git merge-base master current)'";
      yolo = ''git commit -m "$(curl -s http://whatthecommit.com/index.txt)" '';
      recent = "ls -Art | tail -n 1";
      cdnix = "cd /etc/nixos";
      gitc = "git commit -m";
      gitch = "git checkout";
      gits = "git status";
      ssh = "TERM=xterm-color ssh";
      svp = "sudo !!";
      cdtmp = "cd $(mktemp -d)";
    };
    history = {
      ignoreSpace = true;
      extended = true;
      save = 50000;
    };
  };

  programs.zsh.oh-my-zsh = {
    enable = true;
    theme = "dpoggi";
    plugins = [
      "git"
      "git-flow"
      "git-extras"
      "pass"
      "docker"
      "docker-compose"
      "ansible"
      "terraform"
      "kubectl"
      "vagrant"
      "npm"
      "node"
      "python"
      "golang"
    ];
    extraConfig = ''
      setopt HIST_IGNORE_SPACE

      export PATH=$HOME/bin:/usr/local/bin:$PATH
      export PATH=$HOME/snap:$PATH
      export PATH=$HOME/.emacs.d/bin/:$PATH

      export PATH="/usr/local/go/bin:$PATH"
      export PATH="$HOME/go/bin:$PATH"
    '';

  };

}
