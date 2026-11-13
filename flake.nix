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

        # WasmCert 2.2.1 rather than the 2.2.0 nixpkgs picks for Rocq 9.1.
        # 2.2.1 fixes signed LEB128 encoding and `return_call` parsing, both
        # of which the binary-parser round-trip depends on -- that round-trip
        # is what is meant to retire the trusted WAT transcription in the
        # examples (PLAN-authentikate.md F(5)).  Overriding the scope, not a
        # single reference, so the dev shell, wasmglue and wasmcomponents all
        # see one WasmCert.
        rocqPackages = pkgs.rocqPackages_9_1.overrideScope (
          final: prev: {
            wasmcert = prev.wasmcert.override { version = "2.2.1"; };
          }
        );
        rocq-core = rocqPackages.rocq-core;
        ocaml = rocq-core.ocamlPackages.ocaml;
        vsrocq-language-server = rocqPackages.vsrocq-language-server;

        # The glue layer is its own package (rocq-wasmcomponents-certirocq):
        # nothing in it depends on the component-model formalization, and
        # verifying hand-written glue should not require building that.
        # See wasmglue/README.md.
        wasmglue = pkgs.callPackage ./wasmglue { inherit rocqPackages; };

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
            wasmglue
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
        packages.wasmglue = wasmglue;
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

          # This Rocq install has no `coqc` symlink, and rocq-mcp invokes
          # the binary coqc-style (`$BINARY -Q ...`), so a bare `rocq`
          # fails with "Unknown subcommand -Q" — shim in the `c`.
          ROCQ_COQC_BINARY = "${pkgs.writeShellScript "coqc-shim" ''
            exec ${rocq-core}/bin/rocq c "$@"
          ''}";
          shellHook = ''
            # ROCQ_WORKSPACE must be the live checkout, not the store copy
            # of ${self} (which omits untracked files).
            export ROCQ_WORKSPACE="$PWD"
            # wasmglue is a dependency, not a subdirectory of this build, so
            # it is not on the -Q line.  Point the load path at the working
            # tree rather than at `packages.wasmglue`, so that editing
            # wasmglue/WasmGlue and running `make -C wasmglue` is enough.
            # Prepend: the nix setup hook has already put wasmcert (and
            # compcert, through it) on ROCQPATH, and overwriting it loses them.
            export ROCQPATH="$PWD/wasmglue''${ROCQPATH:+:$ROCQPATH}"
            # stderr, not stdout: `nix develop -c rocq-mcp` speaks JSON-RPC
            # over stdout, and a banner there corrupts the handshake.
            echo "WasmComponents development environment" >&2
            # Not `rocq-mcp --version`: there is no such flag, and it falls
            # through to starting the server, which then eats the shell's stdin.
            echo "  rocq-mcp:          $(command -v rocq-mcp)" >&2
            echo "  ROCQ_COQC_BINARY:  $ROCQ_COQC_BINARY" >&2
            echo "  ROCQ_WORKSPACE:    $ROCQ_WORKSPACE" >&2
            echo "  WasmGlue:          $PWD/wasmglue (on ROCQPATH; build with make -C wasmglue)" >&2
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
