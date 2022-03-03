{ config, lib, pkgs, inputs, ... }:

{
  environment.systemPackages = with pkgs; [
    qemu_kvm
    wget
    curl
    vim
    ack
    tree
    fd
    ripgrep
    bc
    finger_bsd
    gnupg
    zip
    unzip
    rsync
    htop
    gotop
    lsof
    pstree
    parted
    traceroute
    inetutils
    tcpdump
    whois
    dnsutils
    mtr
    docker-compose
    xorg.xhost
    unrar
    element-desktop
    woeusb
    tldr
    yubioath-desktop
    # virt-manager
    xorg.xmessage
    gdk-pixbuf
    librsvg
    gnumake
    cmake
    pciutils
    killall
    libnotify
    gcc

    pkg-config
    openconnect

    jre
    adoptopenjdk-openj9-bin-16

    openmoji-color
  ];

  nixpkgs.overlays = [
    (final: prev: {
      nextcloud-client = (prev.nextcloud-client.overrideAttrs (o: rec {
        # does not find password
        # https://github.com/NixOS/nixpkgs/pull/113731
        # In case nextcloud will not remember passwords...
        qtWrapperArgs = [
          "--prefix LD_LIBRARY_PATH : ${
            prev.lib.makeLibraryPath [ prev.libsecret ]
          }"
        ];
      }));
    })
  ];

}
