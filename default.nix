with import <nixpkgs> {};

haskellPackages.developPackage {
  root = ./.;
  overrides = self: super: {
    discord-haskell = self.callPackage
      ({ mkDerivation, aeson, async, base, base64-bytestring, bytestring
       , containers, data-default, http-client, iso8601-time, JuicyPixels
       , MonadRandom, req, safe-exceptions, text, time
       , unordered-containers, vector, websockets, wuss
       }:
       mkDerivation {
         pname = "discord-haskell";
         version = "0.7.1";
         sha256 = "0cl40ph5qwpxa05q7jr67syq9dijxyzvmqzgw53wfri4800qxphn";
         revision = "1";
         editedCabalFile = "022rnkpy9frsn81d2m9n8r5crsjzjk679mfja5d65s5bzzg3plyj";
         libraryHaskellDepends = [
           aeson async base base64-bytestring bytestring containers
           data-default http-client iso8601-time JuicyPixels MonadRandom req
           safe-exceptions text time unordered-containers vector websockets
           wuss
         ];
         description = "Write bots for Discord in Haskell";
         license = stdenv.lib.licenses.mit;
         hydraPlatforms = stdenv.lib.platforms.none;
       }) {};
  };
}
