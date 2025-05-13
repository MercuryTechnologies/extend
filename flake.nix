{ inputs.all-cabal-hashes = {
    url = "github:commercialhaskell/all-cabal-hashes/hackage";

    flake = false;
  };

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { all-cabal-hashes, flake-utils, nixpkgs, self }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        config.allowUnfree = true;

        pkgs = import nixpkgs {
          inherit config system;

          overlays = [ self.overlays.default ];
        };

      in
        { packages.default = pkgs.haskellPackagesCustom.extend;

          devShells.default = pkgs.haskellPackagesCustom.shellFor {
            packages = hpkgs: [
              hpkgs.extend
            ];

            nativeBuildInputs = [
              pkgs.haskell-language-server

              (pkgs.vscode-with-extensions.override {
                vscodeExtensions = [
                  pkgs.vscode-extensions.haskell.haskell
                  pkgs.vscode-extensions.justusadam.language-haskell
                ];
              })
            ];

            withHoogle = true;

            doBenchmark = true;
          };
        }
    ) // {
      overlays.default = self: super: {
        inherit all-cabal-hashes;

        haskellPackagesCustom = self.haskellPackages.override (old: {
          overrides =
            let
              hlib = self.haskell.lib.compose;
            in
              self.lib.composeManyExtensions [
                (hlib.packageSourceOverrides {
                  extend = ./.;
                })

                (hlib.packagesFromDirectory {
                  directory = ./dependencies;
                })

                (hself: hsuper: {
                  # Add any package-specific overrides here if needed
                })
              ];
        });
      };
    };
} 