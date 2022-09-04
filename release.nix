let
    pkgs = import <nixpkgs> { };
    star-data = import ./star-data.nix {};
    stars = star-data.hyg-star-data;
    lines = star-data.constellation-lines;
in
  rec {
    program = pkgs.haskellPackages.callPackage ./default.nix { };
    chart = pkgs.stdenv.mkDerivation {
      name = "star-chart.svg";
      program = program;
      stars = stars;
      lines = lines;
      buildCommand = ''
        declare -xp
        $program/bin/star-chart \
            --star_file $stars \
            --constellation_file $lines \
            --out_path $out
      '';
    };
  }
