{ mkDerivation, base, containers, hashable, heaps, lib, matrix, mtl
, PSQueue, split, unordered-containers
}:
mkDerivation {
  pname = "advent-of-code";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers hashable heaps matrix mtl PSQueue split
    unordered-containers
  ];
  executableHaskellDepends = [ base ];
  doHaddock = false;
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
