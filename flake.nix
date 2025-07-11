{
  inputs = { nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable"; };

  outputs = inputs:
    let
      system = "x86_64-linux";
      pkgs = import inputs.nixpkgs { inherit system; };
    in {
      devShells.${system} = {
        default = pkgs.mkShell {
          packages = with pkgs; [
            just
            ocaml
            ocamlPackages.ocaml-lsp
            ocamlPackages.ocamlformat
            ocamlPackages.dune_3
            nixfmt-classic
          ];
          shellHook = ''
            echo 'Hello from minikast devshell'
            export OCAML_BACKTRACE=1
            export OCAMLRUNPARAM=b
            export DUNE_CONFIG__GLOBAL_LOCK=disabled
          '';
        };
      };
      formatter.${system} = pkgs.nixfmt-classic;
    };
}
