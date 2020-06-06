# my learning journal with nix and Haskell

## tutorial link: https://cah6.github.io/technology/nix-haskell-1/

## dev and build with nix

1. ghcid `nix-shell shell2.nix --run 'ghcid -W -c "cabal new-repl" -T Main.main'`
2. run vs code inside nix shell `nix-shell shell2.nix` and then `code .`
3. fire up a repl `nix-shell shell2.nix --run "cabal new-repl"`
4. start Hoogle `nix-shell shell2.nix --run --arg withHoogle true "hoogle server --local --port=8080"`
5. build `nix-shell shell2.nix --run "cabal new-build "`
