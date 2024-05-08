{
  description = "A flake for a chip8 emulator";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
    pkgName = "chip8";
    haskPkgs = pkgs.haskell.packages.ghc965;

  in {
    packages.${system}.${pkgName} = haskPkgs.developPackage { root  = ./.; };
    defaultPackage.${system} = self.packages.${system}.${pkgName};

    devShells = {
      ${system}.default = pkgs.mkShell {
        buildInputs = [
          ( haskPkgs.ghcWithPackages ( p: [
            p.haskell-language-server
            p.ghcid
            p.cabal-install
            p.GLUT
          ]))
        ];
      };
    };
  };
}
