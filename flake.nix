{
  description = "Haskell env";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    home = {
      url = "path:/home/secminhr";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, home }:
    let system = "aarch64-linux";
        pkgs = nixpkgs.legacyPackages.${system};
        commonTools = import "${home}/nvim.nix" { inherit pkgs; };
    in {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = commonTools ++ (with pkgs; [
          haskell-language-server
        ]);
      };
    };
}
