{ mkDerivation }:
mkDerivation {
  pname = "lamh";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [];
  executableHaskellDepends = [];
  license = stdenv.lib.licenses.bsd3;
}
