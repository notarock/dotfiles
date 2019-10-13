# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.cleanTmpDir = true;

  # Virtualization
  virtualisation.docker.enable = true;
  virtualisation.virtualbox.host.enable = true;
  virtualisation.virtualbox.guest.enable = true;

  networking.hostName = "thinknix"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;
  hardware.bluetooth.enable = true;

networking.extraHosts =
  ''
 192.168.10.10 homestead.test
  '';


  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_CA.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "America/Montreal";

  # CLI stuff
  programs.zsh.enable = true;
  programs.zsh.ohMyZsh.enable = true;


  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [ 
	# CLI Stuff
	wget curl 
	vim 
	git tig
	ack tree
	feh neofetch scrot
	gnupg pass  
	zip unzip unar
	sbcl php
	#
	# Tooling
	#
	kitty tilda
	docker-compose vagrant
	#
	# GUI Apps
	#
	emacs
	libreoffice evince
	firefox thunderbird
	qtpass
	obs-studio
	#
	# Eye candy
	#
	moka-icon-theme screenkey
	texlive.combined.scheme-full
  ];

	# Some programs need SUID wrappers, can be configured further or are
	# started in user sessions.
	# programs.mtr.enable = true;
	programs.gnupg.agent = {
		enable = true;
		enableSSHSupport = true;
	};

	# list services that you want to enable:
	services = {
		journald.console = "/dev/tty12";
		printing.enable = true;

		# Enable the X11 windowing system.
		xserver = {
			enable = true;
			layout = "ca,fr";
			dpi = 144;

			# Enable touchpad support.
			libinput.enable = true;
			wacom.enable = true;
		};
	};

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # services.xserver.xkbOptions = "eurosign:e";


  # Enable the KDE Desktop Environment.
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;

  # Enable Stump Window Manager 
  services.xserver.windowManager.stumpwm.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.notarock = {
    home = "/home/notarock";
    description = "VIP";
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "docker" ]; # Enable ‘sudo’ for the user.
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment? YES

}
