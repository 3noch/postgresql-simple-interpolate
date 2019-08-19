{ pkgs ? import <nixpkgs> {}, haskellPackages ? pkgs.haskellPackages, ... }:
{
  haskellPackages = pkgs.haskellPackages.extend (self: super:
  let
    gargoylePkg = import (pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "gargoyle";
      rev = "1ddbfd14ecafa5225779967843d31f2a5f964deb";
      sha256 = "032jmfpyg740m5bynjmjcxxp2c773qad9b7494h9jg3vw4hp328g";
    }) self;
  in
  {
    postgresql-simple-interpolate = self.callCabal2nix "postgresql-simple-interpolate" ./. {};
    inherit (gargoylePkg) gargoyle gargoyle-postgresql gargoyle-postgresql-nix gargoyle-postgresql-connect;
  });
}
