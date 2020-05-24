let 
  pinnedPkgs = import ./pkgs-from-json.nix { json = ./nixos-20-03.json; };
in 
  { project1 = pinnedPkgs.haskellPackages.callPackage ./default.nix { };
  }
