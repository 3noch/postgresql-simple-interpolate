{ pkgs ? import <nixpkgs> {}, haskellPackages ? pkgs.haskellPackages, ... }:
{
  haskellPackages = pkgs.haskellPackages.extend (self: super:
  let
    gargoylePkg = import (pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "gargoyle";
      rev = "e6fb0c932a615ec525ac2dab3d16acbf3d77da61";
      sha256 = "032jmfpyg740m5bynjmjcxxp2c773qad9b7494h9jg3vw4hp328g";
    }) self;
  in
  {
    postgresql-simple-interpolate = self.callCabal2nix "postgresql-simple-interpolate" ./. {};
    inherit (gargoylePkg) gargoyle gargoyle-postgresql gargoyle-postgresql-nix gargoyle-postgresql-connect;
  });
}
