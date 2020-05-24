let 
  projectDrv = (import ./release2.nix { withHoogle = true; } ).project1;
in
  projectDrv.env
