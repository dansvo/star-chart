let
    pkgs = import <nixpkgs> { };
    star-data = import ./star-data.nix {};
    stars = star-data.hyg-star-data;
    lines = star-data.constellation-lines;
in
  rec {
    program = pkgs.haskellPackages.callPackage ./default.nix { };
    chart = pkgs.stdenv.mkDerivation {
      name = "star-chart";
      program = program;
      stars = stars;
      lines = lines;
      buildCommand = ''
        declare -xp
        $program/bin/star-chart $stars $lines $out
      '';
    };
  }
