{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;
in
{
  home.sessionPath = [
    "$HOME/bin"
    "$HOME/go/bin"
  ]
  ++ lib.optional isLinux "$HOME/snap";

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    profileExtra = lib.optionalString isDarwin ''
      eval "$(/opt/homebrew/bin/brew shellenv)"
    '';
    # To keep using zsh with nix-shells
    plugins = [
      {
        name = "zsh-nix-shell";
        file = "nix-shell.plugin.zsh";
        src = pkgs.fetchFromGitHub {
          owner = "chisui";
          repo = "zsh-nix-shell";
          rev = "v0.1.0";
          sha256 = "0snhch9hfy83d4amkyxx33izvkhbwmindy0zjjk28hih1a9l2jmx";
        };
      }
    ];
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
      open = lib.mkIf isLinux "$FILEMANAGER";
      nixc = "sudo $EDITOR /etc/nixos/configuration.nix";
      wttr = "curl wttr.in";
      k = "kubectl";
      randpw = "dd if=/dev/urandom bs=1 count=64 2>/dev/null | ${pkgs.coreutils}/bin/base64 -w 0 | rev | cut -b 2- | rev";
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
    initContent = ''
      # Create a git worktree from the latest main branch.
      # Usage: worktree-for <branch-name>
      # Worktree is created at ~/src/worktrees/<reponame>-<branch-name>.
      # If branch-name matches an existing remote branch it is checked out;
      # otherwise a new branch is created from origin/main.
      worktree-for() {
        if [[ $# -ne 1 ]]; then
          echo "Usage: worktree-for <branch-name>" >&2
          return 1
        fi
        local branch="$1"
        local repo
        repo=$(basename "$(git rev-parse --show-toplevel)")
        local worktree_path="$HOME/src/worktrees/''${repo}-''${branch}"

        git fetch origin main

        if git ls-remote --exit-code --heads origin "$branch" &>/dev/null; then
          git worktree add "$worktree_path" "$branch"
        else
          git worktree add -b "$branch" "$worktree_path" origin/main
        fi
      }
    '';
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

      export KUBECTL_EXTERNAL_DIFF="colordiff -N -u"
    '';

  };

}
