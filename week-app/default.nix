{ mkDerivation, aeson, base, bytestring, hpack, lucid, safe
, servant-lucid, servant-server, stdenv, text, time, warp
}:
mkDerivation {
  pname = "week-app";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring lucid safe servant-lucid servant-server text
    time warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring lucid safe servant-lucid servant-server text
    time warp
  ];
  testHaskellDepends = [
    aeson base bytestring lucid safe servant-lucid servant-server text
    time warp
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/prasmussen/week-app#readme";
  license = stdenv.lib.licenses.bsd3;
}
