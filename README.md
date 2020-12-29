# notarock-nixconfig
NixOS Configurations files

## To install

If the hardware-configuration is already generated, simply simlink the
corresponding `/hosts/<host>` to the folder `host` like this:

``` sh
ln -s hosts/<name> host
```

Otherwise, follow the nixos installation guide and keep the
*hardware-configuration.nix* file inside of a corresponding folder under
*hosts*.
