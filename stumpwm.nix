{ pkgs, ... }:

{
  services.xserver.windowManager.stumpwm.enable = true;

  environment.systemPackages = with pkgs; [

    lispPackages.xembed
    lispPackages.clx
    lispPackages.clx-truetype
    lispPackages.xkeyboard

    (lib.overrideDerivation lispPackages.stumpwm (x: {

      linkedSystems = x.linkedSystems ++ [
        "clx-truetype"
        "xkeyboard"
        "xembed"
        "clx"
      ];

      buildInputs = x.buildInputs ++ (with lispPackages; [
        clx-truetype
        xkeyboard
        xembed
        clx
      ]);

    }))
  ];

}
