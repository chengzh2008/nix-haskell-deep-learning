let 
  pinnedPkgs = import ./pkgs-from-json.nix { json = ./nixos-20-03.json; };
  myPackages = (import ./release2.nix { withHoogle = true; } );

  projectDrvEnv = myPackages.project1.env.overrideAttrs (oldAttrs: rec 
    {
      buildInputs = oldAttrs.buildInputs ++ [
        # required!
        pinnedPkgs.haskellPackages.cabal-install
        # optional
        pinnedPkgs.haskellPackages.hlint
        ];
      shellHook = ''
        export USERNAME="christian.henry"
      '';
    });
in 
  projectDrvEnv
