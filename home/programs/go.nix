{ config, lib, pkgs, inputs, osConfig, ... }:

{
  programs = {
    go = {
      enable = true;
      package = pkgs.go_1_24;
      packages = {
        "github.com/motemen/gore/cmd/gore" = inputs.gore;
        "github.com/mdempsky/gocode" = inputs.gotools;
        "golang.org/x/tools/cmd/goimports" = inputs.gotools;
        "golang.org/x/tools/cmd/godoc" = inputs.gotools;
        "golang.org/x/tools/cmd/gorename" = inputs.gotools;
        "golang.org/x/tools/cmd/guru" = inputs.gotools;
        "github.com/cweill/gotests/..." = inputs.gotests;
        "github.com/fatih/gomodifytags" = inputs.gomodifytags;
      };
    };
  };
}
