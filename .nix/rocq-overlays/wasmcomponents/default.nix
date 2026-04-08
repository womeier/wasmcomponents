{
  lib,
  mkRocqDerivation,
  rocq-core,
  stdlib,
  wasmcert,
  version ? null,
}:

with lib;
mkRocqDerivation {
  pname = "wasmcomponents";
  repo = "wasmcomponents";
  owner = "womeier";

  inherit version;
  defaultVersion = "0.0.1";

  mlPlugin = true;

  propagatedBuildInputs = [
    stdlib
    wasmcert
  ];

  releaseRev = v: "v${v}";

  meta = {
    description = "Formalization of the Wasm component standard in Rocq";
    maintainers = with maintainers; [ womeier ];
    license = licenses.mit;
  };
}
