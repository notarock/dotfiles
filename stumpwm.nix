{ pkgs, ... }:

{
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

  environment.variables = {
    ASDF_OUTPUT_TRANSLATIONS="/nix/store/:/nix/store/";
  };

  services.xserver.windowManager.stumpwm.enable = true;


}
