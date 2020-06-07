{ mkDerivation, base, containers, lens, stdenv, text, zlib }:
mkDerivation {
  pname = "nix-haskell";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers lens text zlib ];
  license = stdenv.lib.licenses.bsd3;
}
