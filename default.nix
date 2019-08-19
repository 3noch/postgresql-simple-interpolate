{ pkgs ? import <nixpkgs> {}, haskellPackages ? pkgs.haskellPackages, ... }:
let
  hsOnly = pkg: pkg.overrideAttrs ({ src, ... }: {
    src = pkgs.lib.cleanSourceWith {
      filter = (name: type: let baseName = baseNameOf (toString name); in
        type == "directory" && !(builtins.elem baseName ["dist" "dist-newstyle"]) ||
        builtins.elem baseName ["LICENSE"] ||
        pkgs.lib.hasSuffix ".hs" baseName ||
        pkgs.lib.hasSuffix ".cabal" baseName ||
        pkgs.lib.hasSuffix ".nix" baseName ||
        pkgs.lib.hasSuffix ".md" baseName
      );
      src = pkgs.lib.cleanSource (pkgs.lib.cleanSource src);
    };
  });

  checkHLint = pkg: pkg.overrideAttrs ({ preConfigure ? "", src, ... }: {
    preConfigure = ''
      (
        echo "Checking for lint"

        set -x
        '${pkgs.hlint}/bin/hlint' --version
        '${pkgs.hlint}/bin/hlint' --hint '${builtins.path { path = ./.hlint.yaml; name = "hlint.yaml"; }}' '${src}' || exit 1
      )
      ${preConfigure}
    '';
  });
in {
  haskellPackages = haskellPackages.extend (self: super:
  let
    gargoylePkg = import (pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "gargoyle";
      rev = "1ddbfd14ecafa5225779967843d31f2a5f964deb";
      sha256 = "032jmfpyg740m5bynjmjcxxp2c773qad9b7494h9jg3vw4hp328g";
    }) self;
  in
  {
    postgresql-simple-interpolate = checkHLint (hsOnly (self.callCabal2nix "postgresql-simple-interpolate" ./. {}));
    inherit (gargoylePkg) gargoyle gargoyle-postgresql gargoyle-postgresql-nix gargoyle-postgresql-connect;
  });
}
