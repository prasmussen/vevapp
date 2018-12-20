{ mkDerivation, aeson, base, bytestring, hpack, http-api-data
, iproute, lucid, network, safe, servant-lucid, servant-server
, stdenv, text, time, warp
}:
mkDerivation {
  pname = "ip-app";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring http-api-data iproute lucid network safe
    servant-lucid servant-server text time warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring http-api-data iproute lucid network safe
    servant-lucid servant-server text time warp
  ];
  testHaskellDepends = [
    aeson base bytestring http-api-data iproute lucid network safe
    servant-lucid servant-server text time warp
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/prasmussen/vevapp#readme";
  license = stdenv.lib.licenses.bsd3;
}
