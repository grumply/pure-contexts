{ mkDerivation, base, pure-elm, stdenv }:
mkDerivation {
  pname = "pure-contexts";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure-elm ];
  homepage = "github.com/grumply/pure-contexts";
  description = "Layerable contexts";
  license = stdenv.lib.licenses.bsd3;
}