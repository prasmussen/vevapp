let
  config = rec {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackgesOld: rec {
          ipApp =
            pkgs.haskell.lib.overrideCabal
              ( haskellPackagesNew.callPackage ./default.nix {}
              )
              ( oldDerivation: {
                  enableSharedExecutables = false;
                }
              );
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
in
{ ipApp = pkgs.haskellPackages.ipApp;
}
