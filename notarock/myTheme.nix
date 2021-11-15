{ config, lib, pkgs, ... }:

with lib;

{
  options.myTheme = mkOption {
    type = types.attrsOf types.str;
    default = { };
    example = literalExpression ''
      {
        color0 = ""#111111";
        ...
        color16 = "#000000";
      }
    '';
    description = "Contains colors theme used in most programs";
  };
}
