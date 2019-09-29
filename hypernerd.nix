{ mkDerivation, aeson, array, async, base, bytestring, cassava
, clock, containers, discord-haskell, exceptions, hookup
, http-conduit, http-types, HUnit, ini, irc-core, JuicyPixels
, louis, network, qm-interpolated-string, random, raw-strings-qq
, regex-base, regex-tdfa, safe, sqlite-simple, stdenv, stm
, template-haskell, temporary, text, time, transformers
, unordered-containers, uri-encode, vector
}:
mkDerivation {
  pname = "HyperNerd";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson array async base bytestring cassava clock containers
    discord-haskell exceptions hookup http-conduit http-types ini
    irc-core JuicyPixels louis network qm-interpolated-string random
    raw-strings-qq regex-base regex-tdfa safe sqlite-simple stm
    template-haskell text time transformers unordered-containers
    uri-encode vector
  ];
  testHaskellDepends = [
    aeson array async base bytestring containers discord-haskell
    exceptions http-conduit http-types HUnit qm-interpolated-string
    raw-strings-qq regex-base regex-tdfa safe sqlite-simple stm
    temporary text time transformers unordered-containers uri-encode
  ];
  homepage = "https://github.com/tsoding/HyperNerd";
  description = "Twitch bot for Tsoding streams";
  license = stdenv.lib.licenses.mit;
}
