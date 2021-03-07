#!/usr/bin/env bash
set -euo pipefail

echo "Gives permission to ssh keys"
sudo chmod 700 ~/.ssh
sudo chmod 644 ~/.ssh/known_hosts
sudo chmod 600 ~/.ssh/id_rsa
sudo chmod 644 ~/.ssh/id_rsa.pub

cd /nixos-configuration && git remote remove origin && \
    git remote add origin git@github.com:notarock/nixos-configuration.git

git clone git@github.com:notarock/pass.git ~/.password-store

ln -s ~/Nextcloud/Pictures/ ~/Pictures
ln -s ~/Nextcloud/Documents/ ~/Documents
ln -s ~/Nextcloud/Videos/ ~/Videos
ln -s ~/Nextcloud/Music ~/Music
