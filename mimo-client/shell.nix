{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, Cabal, containers, lens, miso, stdenv, text, hsx2hs, haskell-src-exts, happy }:
      mkDerivation {
        pname = "miso-editor-client";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ aeson base containers hsx2hs haskell-src-exts lens miso text ];
        buildTools = [ Cabal pkgs.haskellPackages.Cabal pkgs.haskellPackages.cabal-install pkgs.haskellPackages.ghc ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
