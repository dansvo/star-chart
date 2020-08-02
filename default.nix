{ mkDerivation, base, diagrams, diagrams-lib, diagrams-svg
, parallel, parsec, parsec3-numbers, stdenv
}:
mkDerivation {
  pname = "star-chart";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base diagrams diagrams-lib diagrams-svg parallel parsec
    parsec3-numbers
  ];
  homepage = "https://github.com/githubuser/star-chart#readme";
  license = stdenv.lib.licenses.bsd3;
}
