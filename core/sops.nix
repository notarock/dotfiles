{ config, lib, pkgs, ... }:

{
  imports = [ <sops-nix/modules/sops> ];
  # This will add secrets.yml to the nix store
  # You can avoid this by adding a string to the full path instead, i.e.
  # sops.defaultSopsFile = "/root/.sops/secrets.yaml";
  sops.defaultSopsFile = ./secrets.yaml;
  sops.secrets.example-key = { };
  # This is using ssh keys in the age format:
  sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
  # This is using an age key that is expected to already be in the filesystem
  sops.age.keyFile = "/var/lib/sops-nix/key.txt";
  # This will generate a new key if the key specified above does not exist
  sops.age.generateKey = true;
  sops.secrets."myservice/my_subdir/my_secret" = { };
}
