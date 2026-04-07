{
  description = "WasmComp-Cert development environment";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        lib = nixpkgs.lib;
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfreePredicate =
            pkg:
            builtins.elem (lib.getName pkg) [
              "compcert"
            ];
        };

        coqPackages = pkgs.coqPackages_9_1;

        coq = coqPackages.coq;
        wasmcert = coqPackages.wasmcert;

      in
      {
        devShells.default = pkgs.mkShell {
          name = "wasmcomp-cert";
          packages = [
            coq
            wasmcert
            coqPackages.vscoq-language-server
          ];
          shellHook = ''
            echo "WasmComp-Cert development environment"
          '';
        };
      }
    );
}
