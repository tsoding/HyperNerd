{ nixpkgs ? import ./pin-unstable.nix {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage ./hypernerd.nix {
  	discord-haskell = haskellPackages.callPackage ./discord-haskell.nix { };
  	louis = haskellPackages.callPackage ./louis.nix { };
  });

in

  if pkgs.lib.inNixShell then drv.env else drv
