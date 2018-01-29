with import <nixpkgs> {}; {
    Tsoder2Env = stdenv.mkDerivation {
        name = "Tsoder2Env";
        buildInputs = [ ghc stack cabal-install openssl ];
        LD_LIBRARY_PATH="${openssl.out}/lib";
    };
}
