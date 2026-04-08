{
  description = "WasmComponents development environment";
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

        rocqPackages = pkgs.rocqPackages_9_1;
        coqPackages = pkgs.coqPackages_9_1;
        rocq-core = rocqPackages.rocq-core;
        ocaml = rocq-core.ocamlPackages.ocaml;
        vsrocq-language-server = rocqPackages.vsrocq-language-server;

        wasmcert = coqPackages.wasmcert;

        # Build wasmcomponents package, copy-pasted from nix toolbox for now
        wasmcomponents = rocqPackages.mkRocqDerivation {
          pname = "wasmcomponents";
          version = "dev";
          src = lib.cleanSource self;

          mlPlugin = true;

          propagatedBuildInputs = [
            rocqPackages.stdlib
            wasmcert
          ];

          buildPhase = ''
            runHook preBuild

            make
            make src/wit_parser_bin

            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall

            OUTDIR=$out/lib/coq/${rocq-core.version}/user-contrib
            COQLIBINSTALL=$OUTDIR make install

            mkdir -p $out/bin
            cp src/wit_parser_bin $out/bin

            runHook postInstall
          '';

          meta = {
            description = "Formalization of the Wasm component standard in Rocq";
            license = lib.licenses.mit;
          };
        };

      in
      {
        packages.default = wasmcomponents;
        packages.wasmcomponents = wasmcomponents;

        devShells.default = pkgs.mkShell {
          name = "wasmcomponents";
          packages = [
            rocq-core
            wasmcert
            vsrocq-language-server
            ocaml
          ];
          shellHook = ''
            echo "WasmComponents development environment"
          '';
        };

        devShells.test-shell = pkgs.mkShell {
          name = "wasmcomponents-test";
          packages = [
            wasmcomponents
            pkgs.python3
            (pkgs.python3.withPackages (ps: [ ps.tqdm ]))
          ];
        };
      }
    );
}
