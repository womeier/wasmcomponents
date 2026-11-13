# The glue library as its own package.  The flake calls this with the
# pinned `rocqPackages`; it is callPackage-able on its own so that a
# consumer who only wants the glue layer does not have to build the
# component-model formalization.
#
# The sources live in ./WasmGlue rather than ./theories so that
# `ROCQPATH=<this directory>` is enough to put them on the load path
# under their logical name — which is how the parent project finds them
# during development, without a second copy on the path.
{
  lib,
  rocqPackages,
}:

rocqPackages.mkRocqDerivation {
  pname = "wasmcomponents-certirocq";
  version = "dev";
  src = lib.cleanSource ./.;

  propagatedBuildInputs = [
    rocqPackages.stdlib
    rocqPackages.wasmcert
  ];

  buildPhase = ''
    runHook preBuild
    make -j$NIX_BUILD_CORES
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    # rocq-version ("9.1"), not version ("9.1.1"): the former is what the
    # setup hook of a dependent package puts on ROCQPATH.
    OUTDIR=$out/lib/coq/${rocqPackages.rocq-core.rocq-version}/user-contrib
    COQLIBINSTALL=$OUTDIR make install
    runHook postInstall
  '';

  meta = {
    description = "Verified glue for CertiRocq-compiled Wasm components";
    license = lib.licenses.mit;
  };
}
