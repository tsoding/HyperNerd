with import <nixpkgs> {}; {
    HyperNerdEnv = stdenv.mkDerivation {
        name = "HyperNerdEnv";
        buildInputs = [ ghc
                        cabal-install
                        openssl
                        zlib
                        haskellPackages.ghcid
                        haskellPackages.hindent
                        haskellPackages.hlint
                      ];
        LD_LIBRARY_PATH="${openssl.out}/lib;${zlib}/lib";
    };
}
