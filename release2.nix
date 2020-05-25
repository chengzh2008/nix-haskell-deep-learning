{ withHoogle ? false
}:
let
  pinnedPkgs = import ./pkgs-from-json.nix { json = ./nixos-20-03.json; };

  customHaskellPackages = pinnedPkgs.haskellPackages.override (old: {
    overrides = pinnedPkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
      # project1 = self.callPackage ./default.nix { };
      project1 = self.callCabal2nix "project1" ./nix-haskell.cabal { };
      # addditional overrides go here
    });
  });

  hoogleAugmentedPackages = import ./toggle-hoogle.nix { withHoogle = withHoogle; input = customHaskellPackages; };

in
  { project1 = hoogleAugmentedPackages.project1;
  }
