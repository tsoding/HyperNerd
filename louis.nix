{ mkDerivation, base, bytestring, fetchgit, JuicyPixels, stdenv
, text, vector
}:
mkDerivation {
  pname = "louis";
  version = "0.1.0.2";
  src = fetchgit {
    url = "https://github.com/tsoding/louis";
    sha256 = "1h29b5kbdlv2if1v4hj1s6pr8qfi191w3vy4hicwi0if8m88di2z";
    rev = "bc6b810161cc461a9e3ad0bb360f90440a07f3c6";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bytestring JuicyPixels text vector
  ];
  description = "Turning images into text using Braille font";
  license = stdenv.lib.licenses.mit;
}
