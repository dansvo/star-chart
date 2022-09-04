{ mkDerivation, base, diagrams, diagrams-lib, diagrams-svg
, parallel, parsec, parsec3-numbers, astro, stdenv, lib
}:
mkDerivation {
  pname = "star-chart";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base diagrams diagrams-lib diagrams-svg parallel parsec
    parsec3-numbers astro
  ];
  homepage = "https://github.com/githubuser/star-chart#readme";
  license = lib.licenses.bsd3;
}
