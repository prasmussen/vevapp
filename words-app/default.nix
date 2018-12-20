{ mkDerivation, aeson, base, bytestring, lucid, random-fu, safe
, servant-lucid, servant-server, stdenv, text, warp
}:
mkDerivation {
  pname = "words-app";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring lucid random-fu safe servant-lucid
    servant-server text warp
  ];
  executableHaskellDepends = [
    aeson base bytestring lucid random-fu safe servant-lucid
    servant-server text warp
  ];
  testHaskellDepends = [
    aeson base bytestring lucid random-fu safe servant-lucid
    servant-server text warp
  ];
  homepage = "https://github.com/prasmussen/words-app#readme";
  license = stdenv.lib.licenses.bsd3;
}
