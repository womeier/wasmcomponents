{
  description = "WasmComponents development environment";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
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
        rocq-core = rocqPackages.rocq-core;
        ocaml = rocq-core.ocamlPackages.ocaml;
        vsrocq-language-server = rocqPackages.vsrocq-language-server;

        # Build wasmcomponents package, copy-pasted from nix toolbox for now
        wasmcomponents = rocqPackages.mkRocqDerivation {
          pname = "wasmcomponents";
          version = "dev";
          src = lib.cleanSource self;

          mlPlugin = true;

          propagatedBuildInputs = [
            rocqPackages.stdlib
            rocqPackages.wasmcert
            rocqPackages.ExtLib
          ];

          buildPhase = ''
            runHook preBuild

            make
            make src/wc-tools

            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall

            OUTDIR=$out/lib/coq/${rocq-core.version}/user-contrib
            COQLIBINSTALL=$OUTDIR make install

            mkdir -p $out/bin
            cp src/wc-tools $out/bin

            runHook postInstall
          '';

          meta = {
            description = "Formalization of the Wasm component standard in Rocq";
            license = lib.licenses.mit;
          };
        };

        # Local pytanque derivation (not yet in nixpkgs).  Wraps the
        # `pet` binary from coq-lsp as a Python library.  pytanque's
        # pyproject.toml has no [build-system], so we patch one in.
        pytanque = pkgs.python3.pkgs.buildPythonPackage rec {
          pname = "pytanque";
          version = "0.2.2";
          format = "pyproject";

          src = pkgs.fetchFromGitHub {
            owner = "LLM4Rocq";
            repo = "pytanque";
            rev = "4092b1238b56468fdc1b3d100e078791c9690fd4"; # v0.2.2
            hash = "sha256-1Hae21BuMdE6MjRdiBO7fcsuS4HzahOdLLhynAUox3I=";
          };

          preBuild = ''
            cat >> pyproject.toml <<'EOF'

[build-system]
requires = ["setuptools>=61.0"]
build-backend = "setuptools.build_meta"

[tool.setuptools.packages.find]
where = ["."]
EOF
          '';

          nativeBuildInputs = [ pkgs.python3.pkgs.setuptools ];

          propagatedBuildInputs = with pkgs.python3.pkgs; [
            typing-extensions
            requests
          ];

          meta = {
            description = "Python wrapper for the pet (coq-lsp) interactive backend";
            license = lib.licenses.asl20;
          };
        };

        # Local rocq-mcp derivation (not yet in nixpkgs).  MCP server for
        # the Rocq prover.  Installed with pip because
        # [fastmcp>=3.1.0] is a meta-package that pulls in
        # [fastmcp-slim] (and several other git/PyPI sources), none of
        # which are in nixpkgs at the required versions.
        #
        # Sandboxed nix builds have no network access, so we split the
        # build: first pre-fetch the full dependency closure into a
        # wheelhouse using a fixed-output derivation (FODs may access the
        # network), then install from that wheelhouse offline.
        rocq-mcp-src = pkgs.fetchFromGitHub {
          owner = "LLM4Rocq";
          repo = "rocq-mcp";
          rev = "6983113d0844c0b7f987c79dab13988445109bfb";
          hash = "sha256-rFtpCnrudC5U3PlL8WfYk23id7T1v1+xL+xQq+riWXY=";
        };

        rocq-mcp-wheelhouse = pkgs.stdenv.mkDerivation {
          name = "rocq-mcp-wheelhouse-0.3.1";
          src = rocq-mcp-src;
          nativeBuildInputs = [
            (pkgs.python3.withPackages (ps: [ ps.pip ]))
            pkgs.git
            pkgs.cacert
          ];
          dontConfigure = true;

          buildPhase = ''
            runHook preBuild
            # /etc/ssl/certs is absent in the sandbox — use the nix cacert store path.
            export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
            export GIT_SSL_CAINFO="$SSL_CERT_FILE"
            # Download the whole resolution closure (incl. the pytanque git
            # dep and the setuptools build backend) for offline install later.
            python3 -m pip download . setuptools wheel \
              -d "$out" --no-input --disable-pip-version-check \
              --progress-bar off
            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall
            touch "$out/.keep"
            runHook postInstall
          '';

          outputHashMode = "recursive";
          outputHashAlgo = "sha256";
          outputHash = "sha256-jBPERlcQD5jJ1LP2sOIB3KQIFDvhLEpWpbM7+m3O3uM=";
        };

        rocq-mcp = pkgs.stdenv.mkDerivation {
          name = "rocq-mcp-0.3.1";
          src = rocq-mcp-src;
          nativeBuildInputs = [ pkgs.python3 pkgs.makeWrapper ];
          dontConfigure = true;

          # Stage 1 has already built a pytanque wheel; drop the git URL
          # from the manifest so the offline resolve matches that wheel.
          patchPhase = ''
            sed -i 's|pytanque @ git+[^"]*|pytanque|' pyproject.toml
          '';

          buildPhase = ''
            runHook preBuild
            python3 -m venv "$out/venv"
            "$out/venv/bin/pip" install --no-index --find-links ${rocq-mcp-wheelhouse} .
            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall
            mkdir -p "$out/bin"
            makeWrapper "$out/venv/bin/rocq-mcp" "$out/bin/rocq-mcp"
            runHook postInstall
          '';

          dontFixup = true;
          meta = {
            description = "MCP server for the Rocq prover";
            license = lib.licenses.asl20;
            mainProgram = "rocq-mcp";
          };
        };

      in
      {
        packages.default = wasmcomponents;
        packages.wasmcomponents = wasmcomponents;
        packages.rocq-mcp = rocq-mcp;
        packages.rocq-mcp-wheelhouse = rocq-mcp-wheelhouse;
        packages.pytanque = pytanque;

        devShells.default = pkgs.mkShell {
          name = "wasmcomponents";
          packages = [
            rocq-core
            # provides the `pet` binary rocq-mcp's interactive tools need.
            rocqPackages.coq-lsp
            rocqPackages.wasmcert
            vsrocq-language-server
            ocaml
            rocq-mcp
          ];

          # This Rocq install has no `coqc` symlink — point rocq-mcp at
          # the `rocq` binary directly.
          ROCQ_COQC_BINARY = "${rocq-core}/bin/rocq";
          shellHook = ''
            # ROCQ_WORKSPACE must be the live checkout, not the store copy
            # of ${self} (which omits untracked files).
            export ROCQ_WORKSPACE="$PWD"
            echo "WasmComponents development environment"
            echo "  rocq-mcp:          $(rocq-mcp --version 2>/dev/null || echo 'available')"
            echo "  ROCQ_COQC_BINARY:  $ROCQ_COQC_BINARY"
            echo "  ROCQ_WORKSPACE:    $ROCQ_WORKSPACE"
          '';
        };

        devShells.test-shell = pkgs.mkShell {
          name = "wasmcomponents-test";
          packages = [
            wasmcomponents
            pkgs.wasm-tools
            pkgs.python3
            (pkgs.python3.withPackages (ps: [ ps.tqdm ]))
          ];
        };
      }
    );
}
