{ pkgs ? (import <nixpkgs> {}) }:
{
  hyg-star-data = pkgs.stdenv.mkDerivation {
    name = "star-data";
    raw_data = builtins.fetchGit {
      url = "git@github.com:astronexus/HYG-Database.git";
      rev = "64a136923f79a657dfe83fa1795f9f4ad1a1bcae";
    };
    buildCommand = ''
      cp $raw_data/hygdata_v3.csv $out
    '';
  };
  constellation-lines = pkgs.stdenv.mkDerivation {
    name = "constellation-lines";
    raw_data = builtins.fetchurl {
      url = "https://raw.githubusercontent.com/Stellarium/stellarium/2e2555863fbcadc965ad21af9388a205e2701f68/skycultures/western_rey/constellationship.fab";
      sha256 = "1w55wr1crh3aiinpdyyzbka897k0c9k6il9izzhcipwis7jfsxpb";
    };
    buildCommand = ''
      cp $raw_data $out
    '';
  };
}
