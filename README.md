# notarock-nixconfig
NixOS Configurations files

## To install

If the hardware-configuration is already generated, simply simlink the corresponding `/hosts/<host>` to the folder `host` like this:

``` sh
ln -s hosts/thinkpad-t480 host
```

If not, run 

``` sh
nixos-generate-config --root /mnt
mkdir /hosts/<computer-identifier>
mv hardware-configuration /hosts/<computer-identifier>
ln -s /hosts/<computer-identifier> host
```

Voilà. You are now ready to install/rebuld. Enjoy

``` sh
nixos-install
```

or

``` sh
nixos-rebuild switch
```

## Using fonts

- Download the font from [here](https://input.fontbureau.com/download/)
- run `nix-store --add-fixed sha256 Input-Font.zip`
- Unzip in `~/.local/share/fonts`
