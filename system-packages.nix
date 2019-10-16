{ pkgs, ... }:

{
  # $ nix search wget
  environment.systemPackages = with pkgs; [
	  # CLI Stuff
	  wget curl
	  vim neovim emacs
	  git tig
	  ack tree exa fd ripgrep bc bat
	  gnupg pass
	  zip unzip
    colordiff
    rsync
    htop gotop
    lsof pstree
    nmap parted ranger stow
    traceroute telnet tcpdump whois dnsutils mtr
	  docker-compose vagrant
	  sbcl
  ];

}

