{ mkDerivation, aeson, async, base, base64-bytestring, bytestring
, containers, data-default, emoji, fetchgit, http-client
, iso8601-time, JuicyPixels, MonadRandom, req, safe-exceptions
, stdenv, text, time, unordered-containers, vector, websockets
, wuss
}:
mkDerivation {
  pname = "discord-haskell";
  version = "0.8.3";
  src = fetchgit {
    url = "https://github.com/aquarial/discord-haskell";
    sha256 = "0q3wq3shxnnr1xyk9y60hmfmyy5n47zdvjx63aq227hdgni5hin3";
    rev = "6bd79875aaaef94d406d5281dfd92a358c91e88c";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base base64-bytestring bytestring containers
    data-default emoji http-client iso8601-time JuicyPixels MonadRandom
    req safe-exceptions text time unordered-containers vector
    websockets wuss
  ];
  executableHaskellDepends = [ base text ];
  homepage = "https://github.com/aquarial/discord-haskell";
  description = "Write bots for Discord in Haskell";
  license = stdenv.lib.licenses.mit;
}
